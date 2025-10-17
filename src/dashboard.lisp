;;;; PAOS - Real-Time Status Dashboard
;;;; Terminal-based UI showing agent progress and status

(in-package #:paos/dashboard)

;;; ============================================================================
;;; ANSI Escape Codes
;;; ============================================================================

(defvar *ansi-clear-screen* (format nil "~C[2J" #\Escape)
  "ANSI code to clear screen.")

(defvar *ansi-home* (format nil "~C[H" #\Escape)
  "ANSI code to move cursor to home position.")

(defvar *ansi-hide-cursor* (format nil "~C[?25l" #\Escape)
  "ANSI code to hide cursor.")

(defvar *ansi-show-cursor* (format nil "~C[?25h" #\Escape)
  "ANSI code to show cursor.")

;; Colors
(defvar *ansi-reset* (format nil "~C[0m" #\Escape))
(defvar *ansi-bold* (format nil "~C[1m" #\Escape))
(defvar *ansi-red* (format nil "~C[31m" #\Escape))
(defvar *ansi-green* (format nil "~C[32m" #\Escape))
(defvar *ansi-yellow* (format nil "~C[33m" #\Escape))
(defvar *ansi-blue* (format nil "~C[34m" #\Escape))
(defvar *ansi-magenta* (format nil "~C[35m" #\Escape))
(defvar *ansi-cyan* (format nil "~C[36m" #\Escape))
(defvar *ansi-white* (format nil "~C[37m" #\Escape))

;;; ============================================================================
;;; Dashboard Core
;;; ============================================================================

(defvar *dashboard-running* nil
  "Flag indicating if dashboard is active.")

(defvar *dashboard-thread* nil
  "Thread running the dashboard update loop.")

(defclass dashboard ()
  ((agents :initarg :agents :accessor dashboard-agents :initform '())
   (width :initarg :width :accessor dashboard-width :initform 80)
   (height :initarg :height :accessor dashboard-height :initform 24)
   (last-update :accessor dashboard-last-update :initform (get-universal-time))))

(defun create-dashboard (&key (width 80) (height 24))
  "Create a new dashboard instance."
  (make-instance 'dashboard :width width :height height))

;;; ============================================================================
;;; Screen Management
;;; ============================================================================

(defun clear-screen ()
  "Clear the terminal screen."
  (format t "~A~A" *ansi-clear-screen* *ansi-home*)
  (force-output))

(defun move-cursor (row col)
  "Move cursor to specific position (1-indexed)."
  (format t "~C[~A;~AH" #\Escape row col)
  (force-output))

(defun hide-cursor ()
  "Hide terminal cursor."
  (format t "~A" *ansi-hide-cursor*)
  (force-output))

(defun show-cursor ()
  "Show terminal cursor."
  (format t "~A" *ansi-show-cursor*)
  (force-output))

(defun colorize (text color &key bold)
  "Apply ANSI color to text."
  (format nil "~A~A~A~A"
          (if bold *ansi-bold* "")
          color
          text
          *ansi-reset*))

;;; ============================================================================
;;; Progress Bar Rendering
;;; ============================================================================

(defun render-progress-bar (percentage &key (width 40) (label ""))
  "Render a progress bar with given percentage (0-100).
Returns formatted string with ANSI codes."
  (let* ((filled (floor (* width (/ percentage 100.0))))
         (empty (- width filled))
         (bar (concatenate 'string
                          (make-string filled :initial-element #\█)
                          (make-string empty :initial-element #\░))))
    (format nil "~A [~A] ~3D%"
            (if (string= label "") "" (format nil "~A:" label))
            (colorize bar (progress-color percentage))
            (floor percentage))))

(defun progress-color (percentage)
  "Get color based on progress percentage."
  (cond
    ((>= percentage 90) *ansi-green*)
    ((>= percentage 50) *ansi-cyan*)
    ((>= percentage 25) *ansi-yellow*)
    (t *ansi-red*)))

(defun render-status-indicator (status)
  "Render a status indicator with color."
  (cond
    ((string-equal status "running") 
     (colorize "●" *ansi-green* :bold t))
    ((string-equal status "pending") 
     (colorize "○" *ansi-yellow*))
    ((string-equal status "done") 
     (colorize "✓" *ansi-green* :bold t))
    ((string-equal status "error") 
     (colorize "✗" *ansi-red* :bold t))
    ((string-equal status "blocked") 
     (colorize "⊗" *ansi-magenta*))
    (t 
     (colorize "?" *ansi-white*))))

;;; ============================================================================
;;; Dashboard Layout
;;; ============================================================================

(defun render-header (dashboard)
  "Render dashboard header."
  (let ((title "PAOS - Parallel Agent Orchestration System"))
    (format t "~A~%"
            (colorize (center-text title (dashboard-width dashboard))
                     *ansi-cyan* :bold t))
    (format t "~A~%~%" (make-string (dashboard-width dashboard) 
                                   :initial-element #\─))))

(defun render-agent-status (agent &key (width 80))
  "Render status for a single agent."
  (let* ((tag (getf agent :tag))
         (status (getf agent :status))
         (progress (or (getf agent :progress) 0))
         (task-count (or (getf agent :task-count) 0))
         (completed (or (getf agent :completed) 0)))
    
    (format t "~A ~A~%" 
            (render-status-indicator status)
            (colorize (format nil "Agent: ~A" tag) *ansi-bold*))
    
    (format t "  ~A~%" 
            (render-progress-bar progress :width 50))
    
    (format t "  Tasks: ~A/~A completed~%~%" 
            completed task-count)))

(defun render-agents-section (dashboard)
  "Render all agents status."
  (format t "~A~%~%" 
          (colorize "Active Agents:" *ansi-cyan* :bold t))
  
  (let ((agents (dashboard-agents dashboard)))
    (if agents
        (dolist (agent agents)
          (render-agent-status agent :width (dashboard-width dashboard)))
        (format t "  ~A~%~%" 
                (colorize "No agents running" *ansi-yellow*)))))

(defun render-summary (dashboard)
  "Render summary statistics."
  (let* ((agents (dashboard-agents dashboard))
         (total-agents (length agents))
         (running-agents (count-if (lambda (a) 
                                    (string-equal (getf a :status) "running"))
                                  agents))
         (total-tasks (reduce #'+ agents 
                             :key (lambda (a) (or (getf a :task-count) 0))
                             :initial-value 0))
         (completed-tasks (reduce #'+ agents
                                :key (lambda (a) (or (getf a :completed) 0))
                                :initial-value 0)))
    
    (format t "~A~%" (make-string (dashboard-width dashboard) 
                                 :initial-element #\─))
    (format t "~A  Agents: ~A/~A active  |  Tasks: ~A/~A completed~%"
            (colorize "Summary:" *ansi-cyan* :bold t)
            running-agents total-agents
            completed-tasks total-tasks)))

(defun render-footer (dashboard)
  "Render dashboard footer with controls."
  (format t "~%~A~%"
          (make-string (dashboard-width dashboard) 
                      :initial-element #\─))
  (format t "~A  ~A~%"
          (colorize "Controls:" *ansi-cyan* :bold t)
          "q=quit | r=refresh | s=switch tab | h=help")
  (format t "Last update: ~A~%"
          (format-timestamp (dashboard-last-update dashboard))))

;;; ============================================================================
;;; Main Rendering
;;; ============================================================================

(defun render-dashboard (dashboard)
  "Render complete dashboard."
  ;; Don't clear if this is first render (to avoid flicker)
  (clear-screen)
  (hide-cursor)
  
  (render-header dashboard)
  (render-agents-section dashboard)
  (render-summary dashboard)
  (render-footer dashboard)
  
  (force-output))

(defun update-dashboard (dashboard agent-statuses)
  "Update dashboard with new agent statuses."
  (setf (dashboard-agents dashboard) agent-statuses)
  (setf (dashboard-last-update dashboard) (get-universal-time))
  (render-dashboard dashboard))

;;; ============================================================================
;;; Real-Time Update Loop
;;; ============================================================================

(defun start-dashboard (agent-status-fn &key (refresh-interval 2))
  "Start dashboard with periodic updates.
agent-status-fn should be a function that returns list of agent statuses."
  (when *dashboard-running*
    (log-warn "Dashboard already running")
    (return-from start-dashboard nil))
  
  (setf *dashboard-running* t)
  (let ((dashboard (create-dashboard)))
    (setf *dashboard-thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (unwind-protect
                  (progn
                    (clear-screen)
                    (loop while *dashboard-running*
                          do (handler-case
                                 (let ((statuses (funcall agent-status-fn)))
                                   (update-dashboard dashboard statuses)
                                   (sleep refresh-interval))
                               (error (e)
                                 (log-error "Dashboard update error: ~A" e)
                                 (sleep refresh-interval)))))
               ;; Cleanup
               (show-cursor)
               (clear-screen)))
           :name "paos-dashboard"))
    dashboard))

(defun stop-dashboard ()
  "Stop the dashboard update loop."
  (when *dashboard-running*
    (setf *dashboard-running* nil)
    (when *dashboard-thread*
      (bordeaux-threads:join-thread *dashboard-thread* :timeout 5)
      (setf *dashboard-thread* nil))
    (show-cursor)
    (log-info "Dashboard stopped")))

;;; ============================================================================
;;; Optimized Rendering (No Flicker)
;;; ============================================================================

(defvar *previous-render* nil
  "Cache of previous render for diff-based updates.")

(defun render-dashboard-optimized (dashboard)
  "Render dashboard with minimal screen updates to prevent flicker."
  (let ((new-render (build-render-buffer dashboard)))
    (if *previous-render*
        (update-changed-lines *previous-render* new-render)
        (progn
          (clear-screen)
          (write-buffer new-render)))
    (setf *previous-render* new-render)
    (force-output)))

(defun build-render-buffer (dashboard)
  "Build dashboard render as list of strings (one per line)."
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (render-header dashboard)
      (render-agents-section dashboard)
      (render-summary dashboard)
      (render-footer dashboard)))
  ;; Split into lines
  (str:lines (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (render-header dashboard)
                 (render-agents-section dashboard)
                 (render-summary dashboard)
                 (render-footer dashboard)))))

(defun update-changed-lines (old-lines new-lines)
  "Update only lines that have changed."
  (loop for row from 1
        for old-line in old-lines
        for new-line in new-lines
        when (not (string= old-line new-line))
        do (progn
             (move-cursor row 1)
             (format t "~C[2K~A" #\Escape new-line))))  ; Clear line and write

(defun write-buffer (lines)
  "Write all lines to screen."
  (move-cursor 1 1)
  (dolist (line lines)
    (format t "~A~%" line)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun center-text (text width)
  "Center text within given width."
  (let* ((text-len (length text))
         (padding (max 0 (floor (- width text-len) 2))))
    (format nil "~v@a~a~v@a" 
            padding "" text 
            (- width text-len padding) "")))

(defun format-timestamp (universal-time)
  "Format timestamp for display."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

(defun truncate-text (text max-length)
  "Truncate text to max length with ellipsis."
  (if (> (length text) max-length)
      (concatenate 'string 
                  (subseq text 0 (- max-length 3))
                  "...")
      text))

;;; ============================================================================
;;; Mock Data for Testing
;;; ============================================================================

(defun create-mock-agent (tag status progress)
  "Create mock agent for testing."
  (list :tag tag
        :status status
        :progress progress
        :task-count 5
        :completed (floor (* 5 (/ progress 100.0)))))

(defun mock-agent-status-fn ()
  "Mock function that returns agent statuses."
  (list
   (create-mock-agent "ui-team" "running" 65)
   (create-mock-agent "backend-team" "running" 40)
   (create-mock-agent "database-team" "done" 100)
   (create-mock-agent "testing-team" "pending" 0)))

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
;;; Demo Function
;;; ============================================================================

(defun demo-dashboard ()
  "Run dashboard demo with mock data."
  (log-info "Starting dashboard demo (press Ctrl+C to stop)...")
  (start-dashboard #'mock-agent-status-fn :refresh-interval 1))

;;; ============================================================================
;;; Package Exports
;;; ============================================================================
;;; Note: All exports are handled in core/package.lisp
