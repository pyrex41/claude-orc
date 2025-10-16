;;;; PAOS - Task Decomposition Engine
;;;; Integration with Taskmaster CLI for PRD decomposition

(in-package #:paos/core)

(defvar *taskmaster-command* "task-master"
  "The command to run Taskmaster CLI.")

(defvar *default-parse-options* '("--num-tasks" "10")
  "Default options for task-master parse-prd command.")

(defun decompose-prd (prd-file &key (num-tasks 10) (tag "master") (research nil))
  "Decompose a PRD file into tasks using Taskmaster CLI.
Returns the list of generated tasks or NIL on error."
  (handler-case
      (let* ((initial-count (count-tasks-in-tag tag))
             (success (call-taskmaster-parse prd-file
                                           :num-tasks num-tasks
                                           :tag tag
                                           :research research)))
        (when success
          (let ((final-count (count-tasks-in-tag tag)))
            (when (> final-count initial-count)
              (get-tasks-in-tag tag)))))
    (error (e)
      (log-error "Failed to decompose PRD ~A: ~A" prd-file e)
      nil)))

(defun call-taskmaster-parse (prd-file &key num-tasks tag research)
  "Call task-master parse-prd command. Returns T on success, NIL on failure."
  (let* ((args (build-parse-args prd-file
                                :num-tasks num-tasks
                                :tag tag
                                :research research))
         (process (uiop:launch-program args
                                     :output :stream
                                     :error-output :stream)))
    (unwind-protect
         (let ((exit-code (uiop:wait-process process)))
           (if (= exit-code 0)
               t
               (progn
                 (let ((error-msg (read-stream-to-string
                                   (uiop:process-info-error-output process))))
                   (log-error "Taskmaster CLI failed with exit code ~A: ~A"
                             exit-code error-msg))
                 nil)))
      (uiop:close-streams process))))

(defun build-parse-args (prd-file &key num-tasks tag research)
  "Build command line arguments for task-master parse-prd."
  (let ((args (list *taskmaster-command* "parse-prd" prd-file)))
    (when num-tasks
      (setf args (append args (list "--num-tasks" (write-to-string num-tasks)))))
    (when tag
      (setf args (append args (list "--tag" tag))))
    (when research
      (setf args (append args (list "--research"))))
    args))

(defun count-tasks-in-tag (tag)
  "Count the number of tasks in the specified tag."
  (let ((tasks-data (read-tasks-json)))
    (if tasks-data
        (let ((tag-data (gethash tag (gethash "tags" tasks-data))))
          (if tag-data
              (length (gethash "tasks" tag-data))
              0))
        0)))

(defun get-tasks-in-tag (tag)
  "Get all tasks in the specified tag."
  (let ((tasks-data (read-tasks-json)))
    (when tasks-data
      (let ((tag-data (gethash tag (gethash "tags" tasks-data))))
        (when tag-data
          (gethash "tasks" tag-data))))))

(defun read-tasks-json ()
  "Read and parse the tasks.json file."
  (let ((tasks-file ".taskmaster/tasks/tasks.json"))
    (when (probe-file tasks-file)
      (handler-case
          (with-open-file (stream tasks-file)
            (cl-json:decode-json stream))
        (error (e)
          (log-error "Failed to read tasks.json: ~A" e)
          nil)))))

(defun read-stream-to-string (stream)
  "Read all content from a stream into a string."
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line
          do (write-line line out))))

;; Integration functions for the orchestrator

(defun decompose-prd-with-fallback (prd-file &key (max-retries 3) (num-tasks 10) (tag "master"))
  "Decompose PRD with retry logic and fallback options."
  (loop for attempt from 1 to max-retries
        do (handler-case
               (return (decompose-prd prd-file
                                     :num-tasks num-tasks
                                     :tag tag))
             (error (e)
               (log-warn "PRD decomposition attempt ~A failed: ~A" attempt e)
               (when (= attempt max-retries)
                 (error "All PRD decomposition attempts failed")))))
  nil)

(defun validate-decomposed-tasks (tasks)
  "Validate that the decomposed tasks have the expected structure."
  (and (hash-table-p tasks)
       (gethash "tasks" tasks)
       (listp (gethash "tasks" tasks))))

(defun extract-task-list (decomposed-data)
  "Extract the task list from decomposed PRD data."
  (when (validate-decomposed-tasks decomposed-data)
    (gethash "tasks" decomposed-data)))

;; Utility functions

(defun log-error (format-string &rest args)
  "Log an error message. Placeholder for proper logging system."
  (format *error-output* "ERROR: ~A~%" (apply #'format nil format-string args)))

(defun log-warn (format-string &rest args)
  "Log a warning message. Placeholder for proper logging system."
  (format *error-output* "WARN: ~A~%" (apply #'format nil format-string args)))

;; Test function for development

(defun test-decomposition (sample-prd-file)
  "Test the decomposition functionality with a sample PRD file."
  (let ((result (decompose-prd sample-prd-file :num-tasks 5)))
    (if result
        (progn
          (format t "Successfully decomposed PRD into ~A tasks~%"
                  (length (extract-task-list result)))
          result)
        (progn
          (format t "Failed to decompose PRD~%")
          nil))))
