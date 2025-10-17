;;;; PAOS - Agent Status Protocol
;;;; JSON-based status reporting between agents and orchestrator

(in-package #:paos/status)

;;; ============================================================================
;;; Status File Paths
;;; ============================================================================

(defvar *status-directory* ".paos/status/"
  "Directory for agent status files.")

(defun get-status-file-path (tag)
  "Get the path to a status file for an agent tag."
  (format nil "~A/status-~A.json" *status-directory* tag))

(defun ensure-status-directory ()
  "Ensure status directory exists."
  (ensure-directories-exist *status-directory*))

;;; ============================================================================
;;; Agent Status Structure
;;; ============================================================================

(defclass agent-status ()
  ((tag :initarg :tag :accessor status-tag :type string)
   (status :initarg :status :accessor status-state :type string)
   (progress :initarg :progress :accessor status-progress :initform 0)
   (current-task :initarg :current-task :accessor status-current-task :initform nil)
   (completed-tasks :initarg :completed-tasks :accessor status-completed-tasks :initform '())
   (errors :initarg :errors :accessor status-errors :initform '())
   (last-update :initarg :last-update :accessor status-last-update)
   (metadata :initarg :metadata :accessor status-metadata :initform (make-hash-table :test 'equal))))

(defun create-agent-status (tag &key status (progress 0) current-task)
  "Create a new agent status object."
  (make-instance 'agent-status
                :tag tag
                :status (or status "pending")
                :progress progress
                :current-task current-task
                :last-update (get-universal-time)))

;;; ============================================================================
;;; JSON Serialization
;;; ============================================================================

(defun agent-status-to-json (status-obj)
  "Convert agent status object to JSON-compatible alist."
  (list (cons :tag (status-tag status-obj))
        (cons :status (status-state status-obj))
        (cons :progress (status-progress status-obj))
        (cons :current-task (status-current-task status-obj))
        (cons :completed-tasks (status-completed-tasks status-obj))
        (cons :errors (status-errors status-obj))
        (cons :last-update (status-last-update status-obj))
        (cons :metadata (hash-table-to-alist (status-metadata status-obj)))))

(defun json-to-agent-status (json-alist)
  "Convert JSON alist to agent status object."
  (let ((status (make-instance 'agent-status
                              :tag (cdr (assoc :tag json-alist))
                              :status (cdr (assoc :status json-alist))
                              :progress (or (cdr (assoc :progress json-alist)) 0)
                              :current-task (cdr (assoc :current-task json-alist))
                              :last-update (cdr (assoc :last-update json-alist)))))
    (setf (status-completed-tasks status) 
          (cdr (assoc :completed-tasks json-alist)))
    (setf (status-errors status)
          (cdr (assoc :errors json-alist)))
    (let ((metadata-alist (cdr (assoc :metadata json-alist))))
      (when metadata-alist
        (setf (status-metadata status)
              (alist-to-hash-table metadata-alist))))
    status))

(defun hash-table-to-alist (hash-table)
  "Convert hash table to alist for JSON serialization."
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons (intern (string-upcase key) :keyword) value) alist))
             hash-table)
    alist))

(defun alist-to-hash-table (alist)
  "Convert alist to hash table."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (string-downcase (symbol-name (car pair))) hash)
            (cdr pair)))
    hash))

;;; ============================================================================
;;; Agent: Writing Status Files
;;; ============================================================================

(defun write-agent-status (status-obj &key (directory *status-directory*))
  "Write agent status to JSON file."
  (let* ((tag (status-tag status-obj))
         (file-path (format nil "~A/status-~A.json" directory tag)))
    (ensure-directories-exist directory)
    (handler-case
        (with-open-file (stream file-path
                               :direction :output
                               :if-exists :supersede)
          (let ((json (agent-status-to-json status-obj)))
            (write-string (cl-json:encode-json-to-string json) stream))
          (log-info "Wrote status for agent ~A" tag)
          t)
      (error (e)
        (log-error "Failed to write status for ~A: ~A" tag e)
        nil))))

(defun update-agent-status (tag &key status progress current-task 
                                completed-task error metadata-updates)
  "Update agent status file with new information."
  (let ((status-obj (or (read-agent-status tag)
                       (create-agent-status tag))))
    
    ;; Update fields
    (when status
      (setf (status-state status-obj) status))
    (when progress
      (setf (status-progress status-obj) progress))
    (when current-task
      (setf (status-current-task status-obj) current-task))
    (when completed-task
      (push completed-task (status-completed-tasks status-obj)))
    (when error
      (push error (status-errors status-obj)))
    
    ;; Update metadata
    (when metadata-updates
      (maphash (lambda (key value)
                 (setf (gethash key (status-metadata status-obj)) value))
               metadata-updates))
    
    ;; Update timestamp
    (setf (status-last-update status-obj) (get-universal-time))
    
    ;; Write to file
    (write-agent-status status-obj)))

;;; ============================================================================
;;; Orchestrator: Reading Status Files
;;; ============================================================================

(defun read-agent-status (tag &key (directory *status-directory*))
  "Read agent status from JSON file."
  (let ((file-path (format nil "~A/status-~A.json" directory tag)))
    (when (probe-file file-path)
      (handler-case
          (with-open-file (stream file-path)
            (let ((json (cl-json:decode-json stream)))
              (json-to-agent-status json)))
        (error (e)
          (log-error "Failed to read status for ~A: ~A" tag e)
          nil)))))

(defun poll-agent-statuses (tags &key (directory *status-directory*))
  "Poll status files for multiple agents.
Returns list of agent status objects."
  (mapcar (lambda (tag)
            (or (read-agent-status tag :directory directory)
                (create-agent-status tag :status "unknown")))
          tags))

(defun poll-all-statuses (&key (directory *status-directory*))
  "Poll all status files in the directory.
Returns list of all agent status objects."
  (ensure-directories-exist directory)
  (let ((status-files (directory (format nil "~A/status-*.json" directory)))
        (statuses '()))
    (dolist (file status-files)
      (let ((tag (extract-tag-from-filename (namestring file))))
        (when tag
          (let ((status (read-agent-status tag :directory directory)))
            (when status
              (push status statuses))))))
    statuses))

(defun extract-tag-from-filename (filename)
  "Extract agent tag from status filename."
  (let ((basename (file-namestring filename)))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "status-(.+)\\.json" basename)
      (when match
        (aref groups 0)))))

;;; ============================================================================
;;; Status Change Detection
;;; ============================================================================

(defvar *previous-statuses* (make-hash-table :test 'equal)
  "Cache of previous status for change detection.")

(defun status-changed-p (current-status)
  "Check if status has changed since last check."
  (let* ((tag (status-tag current-status))
         (previous (gethash tag *previous-statuses*)))
    (if previous
        (or (not (string= (status-state current-status)
                         (status-state previous)))
            (not (= (status-progress current-status)
                   (status-progress previous)))
            (not (equal (status-current-task current-status)
                       (status-current-task previous))))
        t)))  ; First time seeing this agent = changed

(defun update-status-cache (status)
  "Update the cached status for an agent."
  (setf (gethash (status-tag status) *previous-statuses*)
        status))

(defun get-changed-statuses (current-statuses)
  "Filter to only statuses that have changed."
  (let ((changed '()))
    (dolist (status current-statuses)
      (when (status-changed-p status)
        (push status changed)
        (update-status-cache status)))
    (nreverse changed)))

;;; ============================================================================
;;; Status Summary and Aggregation
;;; ============================================================================

(defun summarize-statuses (statuses)
  "Create summary of multiple agent statuses."
  (let ((summary (make-hash-table :test 'equal))
        (total (length statuses))
        (running 0)
        (done 0)
        (errors 0)
        (total-progress 0))
    
    (dolist (status statuses)
      (let ((state (status-state status)))
        (cond
          ((string= state "running") (incf running))
          ((string= state "done") (incf done))
          ((string= state "error") (incf errors)))
        (incf total-progress (status-progress status))))
    
    (setf (gethash "total" summary) total)
    (setf (gethash "running" summary) running)
    (setf (gethash "done" summary) done)
    (setf (gethash "errors" summary) errors)
    (setf (gethash "avg_progress" summary) 
          (if (> total 0) (/ total-progress total) 0))
    
    summary))

(defun get-stale-agents (statuses &optional (threshold-seconds 300))
  "Get agents that haven't updated in threshold seconds."
  (let ((now (get-universal-time))
        (stale '()))
    (dolist (status statuses)
      (when (> (- now (status-last-update status)) threshold-seconds)
        (push (status-tag status) stale)))
    stale))

;;; ============================================================================
;;; Status Monitoring Loop
;;; ============================================================================

(defvar *monitor-running* nil
  "Flag indicating if monitor is active.")

(defvar *monitor-thread* nil
  "Thread running the monitor loop.")

(defun start-status-monitor (tags callback-fn &key (interval 2))
  "Start monitoring agent statuses.
callback-fn is called with list of statuses on each poll."
  (when *monitor-running*
    (log-warn "Status monitor already running")
    (return-from start-status-monitor nil))
  
  (setf *monitor-running* t)
  (setf *monitor-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *monitor-running*
                 do (handler-case
                        (let ((statuses (poll-agent-statuses tags)))
                          (funcall callback-fn statuses)
                          (sleep interval))
                      (error (e)
                        (log-error "Monitor error: ~A" e)
                        (sleep interval)))))
         :name "paos-status-monitor"))
  (log-info "Status monitor started"))

(defun stop-status-monitor ()
  "Stop the status monitoring loop."
  (when *monitor-running*
    (setf *monitor-running* nil)
    (when *monitor-thread*
      (bordeaux-threads:join-thread *monitor-thread* :timeout 5)
      (setf *monitor-thread* nil))
    (log-info "Status monitor stopped")))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun format-status-display (status)
  "Format status for human-readable display."
  (format nil "~A: ~A (~A%) - ~A"
          (status-tag status)
          (status-state status)
          (status-progress status)
          (or (status-current-task status) "idle")))

(defun clean-old-status-files (&optional (max-age-seconds 86400))
  "Clean status files older than max-age-seconds (default 24h)."
  (let ((now (get-universal-time))
        (cleaned 0))
    (dolist (file (directory (format nil "~A/status-*.json" *status-directory*)))
      (when (> (- now (file-write-date file)) max-age-seconds)
        (delete-file file)
        (incf cleaned)))
    (log-info "Cleaned ~A old status files" cleaned)
    cleaned))

;;; ============================================================================
;;; Logging
;;; ============================================================================

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

;;; ============================================================================
;;; Integration Example
;;; ============================================================================

(defun demo-status-protocol ()
  "Demonstrate the status protocol."
  ;; Agent side: write status
  (let ((status (create-agent-status "demo-agent" 
                                    :status "running"
                                    :progress 50
                                    :current-task "Processing data")))
    (write-agent-status status)
    
    ;; Orchestrator side: read status
    (let ((read-status (read-agent-status "demo-agent")))
      (format t "Read status: ~A~%" (format-status-display read-status))
      
      ;; Update status
      (update-agent-status "demo-agent"
                          :progress 75
                          :completed-task "Data processing")
      
      ;; Read updated
      (let ((updated (read-agent-status "demo-agent")))
        (format t "Updated: ~A~%" (format-status-display updated))))))
