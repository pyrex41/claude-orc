;;;; PAOS - Conflict Detection
;;;; Proactive identification of merge conflicts and dependency issues

(in-package #:paos/conflicts)

;;; ============================================================================
;;; File Overlap Detection
;;; ============================================================================

(defun detect-conflicts (worktrees)
  "Main entry point for conflict detection.
Returns list of detected conflicts."
  (let ((file-conflicts (find-file-overlaps worktrees))
        (dep-conflicts (find-dependency-conflicts worktrees)))
    (append file-conflicts dep-conflicts)))

(defun find-file-overlaps (worktrees)
  "Find files modified in multiple worktrees."
  (let ((file-map (make-hash-table :test 'equal))
        (conflicts '()))
    
    ;; Build map of file -> list of worktrees
    (dolist (worktree worktrees)
      (let ((modified-files (get-modified-files worktree)))
        (dolist (file modified-files)
          (push worktree (gethash file file-map '())))))
    
    ;; Find files modified in multiple worktrees
    (maphash (lambda (file worktree-list)
               (when (> (length worktree-list) 1)
                 (push (make-file-conflict file worktree-list)
                       conflicts)))
             file-map)
    
    conflicts))

(defun get-modified-files (worktree)
  "Get list of modified files in a worktree."
  (handler-case
      (let* ((path (getf worktree :path))
             (output (uiop:run-program 
                     (list "git" "-C" path "diff" "--name-only" "HEAD")
                     :output :string))
             (staged (uiop:run-program
                     (list "git" "-C" path "diff" "--cached" "--name-only")
                     :output :string)))
        (remove-if #'str:empty? 
                  (append (str:lines output)
                         (str:lines staged))))
    (error (e)
      (log-error "Failed to get modified files for ~A: ~A" 
                (getf worktree :tag) e)
      '())))

(defun make-file-conflict (file worktrees)
  "Create file conflict structure."
  (list :type "file-overlap"
        :severity "high"
        :file file
        :worktrees (mapcar (lambda (w) (getf w :tag)) worktrees)
        :message (format nil "File '~A' modified in ~A worktrees: ~{~A~^, ~}"
                        file
                        (length worktrees)
                        (mapcar (lambda (w) (getf w :tag)) worktrees))))

;;; ============================================================================
;;; Dependency Conflict Detection
;;; ============================================================================

(defun find-dependency-conflicts (worktrees)
  "Analyze task dependencies for conflicts."
  (let ((conflicts '()))
    
    ;; Get tasks for each worktree
    (dolist (worktree worktrees)
      (let* ((tasks (getf worktree :tasks))
             (worktree-conflicts (analyze-task-dependencies tasks worktree)))
        (setf conflicts (append conflicts worktree-conflicts))))
    
    ;; Cross-worktree dependency analysis
    (let ((cross-conflicts (analyze-cross-worktree-deps worktrees)))
      (setf conflicts (append conflicts cross-conflicts)))
    
    conflicts))

(defun analyze-task-dependencies (tasks worktree)
  "Analyze dependencies within a worktree's tasks."
  (let ((conflicts '())
        (task-ids (mapcar (lambda (t) (gethash "id" t)) tasks)))
    
    (dolist (task tasks)
      (let ((deps (gethash "dependencies" task)))
        (when deps
          (dolist (dep deps)
            ;; Check for circular dependencies
            (when (has-circular-dependency-p task tasks)
              (push (make-dependency-conflict
                     "circular"
                     (gethash "id" task)
                     dep
                     (getf worktree :tag))
                    conflicts))
            
            ;; Check for missing dependencies
            (unless (member dep task-ids :test #'equal)
              (push (make-dependency-conflict
                     "missing"
                     (gethash "id" task)
                     dep
                     (getf worktree :tag))
                    conflicts))))))
    
    conflicts))

(defun has-circular-dependency-p (task tasks)
  "Check if task has circular dependencies."
  (let ((visited (make-hash-table :test 'equal))
        (stack (make-hash-table :test 'equal)))
    (labels ((visit (task-id)
               (when (gethash task-id stack)
                 (return-from has-circular-dependency-p t))
               (when (gethash task-id visited)
                 (return-from visit nil))
               
               (setf (gethash task-id stack) t)
               (setf (gethash task-id visited) t)
               
               (let* ((task (find task-id tasks 
                                 :key (lambda (t) (gethash "id" t))
                                 :test #'equal))
                      (deps (when task (gethash "dependencies" task))))
                 (when deps
                   (dolist (dep deps)
                     (visit dep))))
               
               (remhash task-id stack)
               nil))
      
      (visit (gethash "id" task)))))

(defun analyze-cross-worktree-deps (worktrees)
  "Analyze dependencies across worktrees."
  (let ((conflicts '()))
    
    (loop for i from 0 below (length worktrees)
          do (loop for j from (1+ i) below (length worktrees)
                   do (let* ((wt1 (nth i worktrees))
                            (wt2 (nth j worktrees))
                            (tasks1 (getf wt1 :tasks))
                            (tasks2 (getf wt2 :tasks)))
                        
                        ;; Check if tasks in wt2 depend on tasks in wt1
                        (dolist (task2 tasks2)
                          (let ((deps (gethash "dependencies" task2)))
                            (when deps
                              (dolist (dep deps)
                                (when (task-exists-in-worktree-p dep wt1)
                                  (push (make-cross-worktree-conflict
                                        (getf wt2 :tag)
                                        (getf wt1 :tag)
                                        (gethash "id" task2)
                                        dep)
                                        conflicts)))))))))
    conflicts))

(defun task-exists-in-worktree-p (task-id worktree)
  "Check if a task exists in a worktree."
  (let ((tasks (getf worktree :tasks)))
    (find task-id tasks 
          :key (lambda (t) (gethash "id" t))
          :test #'equal)))

(defun make-dependency-conflict (type task-id dep-id worktree-tag)
  "Create dependency conflict structure."
  (list :type (format nil "dependency-~A" type)
        :severity (if (string= type "circular") "critical" "high")
        :task task-id
        :dependency dep-id
        :worktree worktree-tag
        :message (cond
                  ((string= type "circular")
                   (format nil "Circular dependency detected: ~A -> ~A in ~A"
                          task-id dep-id worktree-tag))
                  ((string= type "missing")
                   (format nil "Missing dependency: ~A requires ~A in ~A"
                          task-id dep-id worktree-tag)))))

(defun make-cross-worktree-conflict (from-wt to-wt task-id dep-id)
  "Create cross-worktree dependency conflict."
  (list :type "cross-worktree-dependency"
        :severity "medium"
        :from-worktree from-wt
        :to-worktree to-wt
        :task task-id
        :dependency dep-id
        :message (format nil "Task ~A in ~A depends on ~A in ~A (merge order required)"
                        task-id from-wt dep-id to-wt)))

;;; ============================================================================
;;; Advanced Conflict Detection
;;; ============================================================================

(defun detect-semantic-conflicts (worktrees)
  "Detect semantic conflicts based on code analysis."
  (let ((conflicts '()))
    
    ;; Check for API/interface changes
    (dolist (worktree worktrees)
      (let ((interface-changes (detect-interface-changes worktree)))
        (when interface-changes
          (dolist (other-wt worktrees)
            (unless (eq worktree other-wt)
              (when (may-affect-worktree-p interface-changes other-wt)
                (push (make-semantic-conflict worktree other-wt interface-changes)
                      conflicts)))))))
    
    conflicts))

(defun detect-interface-changes (worktree)
  "Detect changes to public interfaces/APIs."
  (let ((modified (get-modified-files worktree))
        (interface-files '()))
    
    ;; Look for files that likely contain interfaces
    (dolist (file modified)
      (when (or (str:ends-with? ".lisp" file)
                (str:ends-with? ".ts" file)
                (str:ends-with? ".js" file)
                (str:contains? "interface" file)
                (str:contains? "api" file))
        (push file interface-files)))
    
    interface-files))

(defun may-affect-worktree-p (interface-changes worktree)
  "Check if interface changes may affect another worktree."
  ;; Simplified heuristic
  (let ((wt-files (get-modified-files worktree)))
    (some (lambda (iface-file)
            (some (lambda (wt-file)
                    (files-related-p iface-file wt-file))
                  wt-files))
          interface-changes)))

(defun files-related-p (file1 file2)
  "Check if two files are likely related."
  (or (str:contains? (file-namestring file1) (file-namestring file2))
      (str:contains? (file-namestring file2) (file-namestring file1))
      (and (str:contains? "api" file1) (str:contains? "client" file2))
      (and (str:contains? "interface" file1) (str:contains? "impl" file2))))

(defun make-semantic-conflict (worktree1 worktree2 interface-files)
  "Create semantic conflict structure."
  (list :type "semantic-conflict"
        :severity "medium"
        :worktree1 (getf worktree1 :tag)
        :worktree2 (getf worktree2 :tag)
        :interfaces interface-files
        :message (format nil "Interface changes in ~A may affect ~A: ~{~A~^, ~}"
                        (getf worktree1 :tag)
                        (getf worktree2 :tag)
                        interface-files)))

;;; ============================================================================
;;; Conflict Alert Generation
;;; ============================================================================

(defun generate-conflict-alerts (conflicts)
  "Generate user-friendly alerts for detected conflicts."
  (let ((alerts '()))
    
    (dolist (conflict conflicts)
      (let ((alert (make-alert conflict)))
        (push alert alerts)))
    
    ;; Sort by severity
    (sort alerts #'> :key (lambda (a) (severity-to-number (getf a :severity))))))

(defun make-alert (conflict)
  "Create alert structure from conflict."
  (list :severity (getf conflict :severity)
        :type (getf conflict :type)
        :message (getf conflict :message)
        :timestamp (get-universal-time)
        :color (severity-to-color (getf conflict :severity))
        :action (recommend-action conflict)))

(defun recommend-action (conflict)
  "Recommend action for resolving conflict."
  (let ((type (getf conflict :type)))
    (cond
      ((string= type "file-overlap")
       "Review overlapping changes and coordinate merge strategy")
      ((str:starts-with? "dependency-" type)
       "Resolve dependency issues before proceeding")
      ((string= type "cross-worktree-dependency")
       (format nil "Merge ~A before ~A" 
               (getf conflict :to-worktree)
               (getf conflict :from-worktree)))
      ((string= type "semantic-conflict")
       "Review API/interface compatibility")
      (t "Review conflict and take appropriate action"))))

(defun severity-to-number (severity)
  "Convert severity to number for sorting."
  (cond
    ((string= severity "critical") 4)
    ((string= severity "high") 3)
    ((string= severity "medium") 2)
    ((string= severity "low") 1)
    (t 0)))

(defun severity-to-color (severity)
  "Convert severity to ANSI color."
  (cond
    ((string= severity "critical") paos/dashboard::*ansi-red*)
    ((string= severity "high") paos/dashboard::*ansi-red*)
    ((string= severity "medium") paos/dashboard::*ansi-yellow*)
    ((string= severity "low") paos/dashboard::*ansi-cyan*)
    (t paos/dashboard::*ansi-white*)))

;;; ============================================================================
;;; Display and Reporting
;;; ============================================================================

(defun display-conflicts (conflicts)
  "Display conflicts in a formatted way."
  (if conflicts
      (progn
        (format t "~%~A⚠️  CONFLICTS DETECTED: ~A~A~%~%"
                paos/dashboard::*ansi-red*
                (length conflicts)
                paos/dashboard::*ansi-reset*)
        
        (dolist (conflict conflicts)
          (format t "~A[~A] ~A~A~%"
                  (severity-to-color (getf conflict :severity))
                  (string-upcase (getf conflict :severity))
                  (getf conflict :message)
                  paos/dashboard::*ansi-reset*)
          (format t "  → ~A~%~%" (getf conflict :action))))
      (format t "~%~A✓ No conflicts detected~A~%~%"
              paos/dashboard::*ansi-green*
              paos/dashboard::*ansi-reset*)))

(defun export-conflict-report (conflicts output-file)
  "Export conflicts to JSON report file."
  (with-open-file (stream output-file
                         :direction :output
                         :if-exists :supersede)
    (let ((report (list :timestamp (get-universal-time)
                       :conflict-count (length conflicts)
                       :conflicts conflicts)))
      (write-string (cl-json:encode-json-to-string report) stream))))

;;; ============================================================================
;;; Continuous Conflict Monitoring
;;; ============================================================================

(defvar *monitor-conflicts-running* nil)
(defvar *monitor-conflicts-thread* nil)

(defun start-conflict-monitoring (worktrees-fn &key (interval 30) callback)
  "Start continuous conflict monitoring.
worktrees-fn should return list of worktrees to monitor."
  (when *monitor-conflicts-running*
    (log-warn "Conflict monitoring already running")
    (return-from start-conflict-monitoring nil))
  
  (setf *monitor-conflicts-running* t)
  (setf *monitor-conflicts-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *monitor-conflicts-running*
                 do (handler-case
                        (let* ((worktrees (funcall worktrees-fn))
                               (conflicts (detect-conflicts worktrees))
                               (alerts (generate-conflict-alerts conflicts)))
                          (when (and callback conflicts)
                            (funcall callback alerts))
                          (sleep interval))
                      (error (e)
                        (log-error "Conflict monitoring error: ~A" e)
                        (sleep interval)))))
         :name "paos-conflict-monitor"))
  (log-info "Conflict monitoring started (interval: ~As)" interval))

(defun stop-conflict-monitoring ()
  "Stop conflict monitoring."
  (when *monitor-conflicts-running*
    (setf *monitor-conflicts-running* nil)
    (when *monitor-conflicts-thread*
      (bordeaux-threads:join-thread *monitor-conflicts-thread* :timeout 5)
      (setf *monitor-conflicts-thread* nil))
    (log-info "Conflict monitoring stopped")))

;;; ============================================================================
;;; Utilities
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
