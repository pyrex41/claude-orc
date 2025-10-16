;;;; PAOS - Reliability Features
;;;; Fault tolerance, state persistence, and recovery

(in-package #:paos/reliability)

;;; ============================================================================
;;; State Management
;;; ============================================================================

(defvar *state-file* ".paos/state.json"
  "Path to persisted state file.")

(defvar *backup-directory* ".paos/backups/"
  "Directory for state backups.")

(defclass session-state ()
  ((agents :initarg :agents :accessor state-agents :initform '())
   (worktrees :initarg :worktrees :accessor state-worktrees :initform '())
   (config :initarg :config :accessor state-config :initform nil)
   (start-time :initarg :start-time :accessor state-start-time)
   (last-save :accessor state-last-save :initform (get-universal-time))
   (metadata :accessor state-metadata :initform (make-hash-table :test 'equal))))

(defun create-session-state ()
  "Create new session state object."
  (make-instance 'session-state
                :start-time (get-universal-time)))

;;; ============================================================================
;;; Auto-Save with Threading
;;; ============================================================================

(defvar *auto-save-running* nil
  "Flag for auto-save thread.")

(defvar *auto-save-thread* nil
  "Auto-save background thread.")

(defvar *current-session* nil
  "Current session state.")

(defun start-auto-save (&key (interval 60))
  "Start auto-save thread that periodically saves session state.
Default interval is 60 seconds."
  (when *auto-save-running*
    (log-warn "Auto-save already running")
    (return-from start-auto-save nil))
  
  (unless *current-session*
    (setf *current-session* (create-session-state)))
  
  (setf *auto-save-running* t)
  (setf *auto-save-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *auto-save-running*
                 do (handler-case
                        (progn
                          (sleep interval)
                          (save-session *current-session*)
                          (log-info "Auto-saved session state"))
                      (error (e)
                        (log-error "Auto-save failed: ~A" e)))))
         :name "paos-auto-save"))
  
  (log-info "Auto-save started (interval: ~As)" interval))

(defun stop-auto-save ()
  "Stop auto-save thread."
  (when *auto-save-running*
    (setf *auto-save-running* nil)
    (when *auto-save-thread*
      (bordeaux-threads:join-thread *auto-save-thread* :timeout 5)
      (setf *auto-save-thread* nil))
    ;; Final save
    (when *current-session*
      (save-session *current-session*))
    (log-info "Auto-save stopped")))

;;; ============================================================================
;;; State Persistence
;;; ============================================================================

(defun save-session (session &optional (file *state-file*))
  "Persist session state to file."
  (handler-case
      (progn
        (ensure-directories-exist (directory-namestring file))
        
        ;; Create backup before saving
        (when (probe-file file)
          (backup-state file))
        
        ;; Save state
        (with-open-file (stream file
                               :direction :output
                               :if-exists :supersede)
          (let ((json (session-to-json session)))
            (write-string (cl-json:encode-json-to-string json) stream)))
        
        (setf (state-last-save session) (get-universal-time))
        (log-info "Session saved to ~A" file)
        t)
    (error (e)
      (log-error "Failed to save session: ~A" e)
      nil)))

(defun session-to-json (session)
  "Convert session state to JSON-serializable format."
  (list (cons :start-time (state-start-time session))
        (cons :last-save (state-last-save session))
        (cons :agents (state-agents session))
        (cons :worktrees (state-worktrees session))
        (cons :config (state-config session))
        (cons :metadata (hash-table-to-alist (state-metadata session)))))

(defun load-session (&optional (file *state-file*))
  "Load session state from file."
  (when (probe-file file)
    (handler-case
        (with-open-file (stream file)
          (let ((json (cl-json:decode-json stream)))
            (json-to-session json)))
      (error (e)
        (log-error "Failed to load session from ~A: ~A" file e)
        nil))))

(defun json-to-session (json)
  "Convert JSON to session state object."
  (let ((session (create-session-state)))
    (setf (state-start-time session) (cdr (assoc :start-time json)))
    (setf (state-last-save session) (cdr (assoc :last-save json)))
    (setf (state-agents session) (cdr (assoc :agents json)))
    (setf (state-worktrees session) (cdr (assoc :worktrees json)))
    (setf (state-config session) (cdr (assoc :config json)))
    
    (let ((metadata-alist (cdr (assoc :metadata json))))
      (when metadata-alist
        (setf (state-metadata session)
              (alist-to-hash-table metadata-alist))))
    
    session))

;;; ============================================================================
;;; Backup Management
;;; ============================================================================

(defun backup-state (&optional (file *state-file*))
  "Create timestamped backup of state file."
  (ensure-directories-exist *backup-directory*)
  
  (let* ((timestamp (format-timestamp (get-universal-time)))
         (backup-file (format nil "~Astate-~A.json" 
                             *backup-directory*
                             (ppcre:regex-replace-all "[: ]" timestamp "-"))))
    (handler-case
        (progn
          (uiop:copy-file file backup-file)
          (log-info "Backed up state to ~A" backup-file)
          backup-file)
      (error (e)
        (log-error "Failed to backup state: ~A" e)
        nil))))

(defun clean-old-backups (&optional (max-age-days 7))
  "Remove backup files older than max-age-days."
  (let ((max-age-seconds (* max-age-days 86400))
        (now (get-universal-time))
        (cleaned 0))
    
    (dolist (file (directory (format nil "~Astate-*.json" *backup-directory*)))
      (when (> (- now (file-write-date file)) max-age-seconds)
        (delete-file file)
        (incf cleaned)))
    
    (log-info "Cleaned ~A old backups" cleaned)
    cleaned))

;;; ============================================================================
;;; Session Recovery
;;; ============================================================================

(defun recover-session ()
  "Recover session from persisted state file.
Returns recovered session or NIL if no state found."
  (if (probe-file *state-file*)
      (handler-case
          (let ((session (load-session *state-file*)))
            (when session
              (log-info "Recovered session from ~A" *state-file*)
              (log-info "Session started: ~A" 
                       (format-timestamp (state-start-time session)))
              (log-info "Last saved: ~A"
                       (format-timestamp (state-last-save session)))
              (log-info "Agents: ~A, Worktrees: ~A"
                       (length (state-agents session))
                       (length (state-worktrees session)))
              
              ;; Validate recovered state
              (if (validate-session-state session)
                  (progn
                    (setf *current-session* session)
                    session)
                  (progn
                    (log-error "Recovered state is invalid")
                    nil))))
        (error (e)
          (log-error "Session recovery failed: ~A" e)
          (log-info "Attempting to recover from backup...")
          (recover-from-backup)))
      (progn
        (log-info "No saved session found")
        nil)))

(defun recover-from-backup ()
  "Try to recover from most recent backup."
  (let ((backups (directory (format nil "~Astate-*.json" *backup-directory*))))
    (when backups
      (let ((latest (car (sort backups #'> :key #'file-write-date))))
        (log-info "Recovering from backup: ~A" latest)
        (load-session latest)))))

(defun validate-session-state (session)
  "Validate that recovered session state is usable."
  (and session
       (state-start-time session)
       (state-last-save session)
       (< (- (get-universal-time) (state-last-save session))
          (* 24 3600))))  ; Not older than 24 hours

;;; ============================================================================
;;; Agent Restart Logic
;;; ============================================================================

(defun restart-agents (session)
  "Restart agents based on recovered session state."
  (let ((agents (state-agents session))
        (restarted '()))
    
    (dolist (agent agents)
      (let* ((tag (getf agent :tag))
             (worktree (getf agent :worktree))
             (context (getf agent :context))
             (status (getf agent :status)))
        
        ;; Only restart if agent was active
        (when (or (string= status "running")
                  (string= status "initializing"))
          (log-info "Restarting agent: ~A" tag)
          
          (handler-case
              (let ((new-agent (paos/zellij:spawn-agent tag worktree context)))
                (when new-agent
                  (push new-agent restarted)
                  (log-info "Successfully restarted ~A" tag)))
            (error (e)
              (log-error "Failed to restart agent ~A: ~A" tag e))))))
    
    (log-info "Restarted ~A/~A agents" 
             (length restarted) 
             (count-if (lambda (a) 
                        (member (getf a :status) '("running" "initializing")
                               :test #'string=))
                      agents))
    restarted))

(defun update-session-agents (agents)
  "Update current session with agent list."
  (when *current-session*
    (setf (state-agents *current-session*) agents)))

(defun update-session-worktrees (worktrees)
  "Update current session with worktree list."
  (when *current-session*
    (setf (state-worktrees *current-session*) worktrees)))

;;; ============================================================================
;;; Crash Recovery
;;; ============================================================================

(defun setup-crash-handler ()
  "Setup handler to save state on crashes."
  #+sbcl
  (sb-ext:enable-interrupt sb-unix:sigterm
                          (lambda (signal code scp)
                            (declare (ignore signal code scp))
                            (log-warn "Received SIGTERM, saving state...")
                            (when *current-session*
                              (save-session *current-session*))
                            (sb-ext:quit)))
  
  (log-info "Crash handler installed"))

(defun safe-shutdown ()
  "Safely shutdown PAOS with state saving."
  (log-info "Starting safe shutdown...")
  
  ;; Stop all background threads
  (paos/dashboard:stop-dashboard)
  (paos/status:stop-status-monitor)
  (paos/ai:stop-continuous-analysis)
  (paos/conflicts:stop-conflict-monitoring)
  (stop-auto-save)  ; This saves state one final time
  
  (log-info "Safe shutdown complete"))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun hash-table-to-alist (hash-table)
  "Convert hash table to alist."
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons (intern (string-upcase k) :keyword) v) alist))
             hash-table)
    alist))

(defun alist-to-hash-table (alist)
  "Convert alist to hash table."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (string-downcase (symbol-name (car pair))) hash)
            (cdr pair)))
    hash))

(defun format-timestamp (universal-time)
  "Format timestamp."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

(defun log-error (format-string &rest args)
  "Log error message."
  (format *error-output* "ERROR: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-warn (format-string &rest args)
  "Log warning message."
  (format *error-output* "WARN: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
