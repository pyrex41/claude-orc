;;;; PAOS - Tagger Tests
;;;; Test suite for intelligent task tagging

(defpackage #:paos/test-tagger
  (:use #:cl #:paos/tagger)
  (:export #:run-tagger-tests))

(in-package #:paos/test-tagger)

;;; Test Data

(defparameter *test-tasks*
  (list
   (let ((task (make-hash-table :test 'equal)))
     (setf (gethash "id" task) "1")
     (setf (gethash "title" task) "Build React UI components")
     (setf (gethash "description" task) "Create reusable React components for the frontend")
     task)
   (let ((task (make-hash-table :test 'equal)))
     (setf (gethash "id" task) "2")
     (setf (gethash "title" task) "Implement REST API endpoints")
     (setf (gethash "description" task) "Create backend API using Express")
     task)
   (let ((task (make-hash-table :test 'equal)))
     (setf (gethash "id" task) "3")
     (setf (gethash "title" task) "Design database schema")
     (setf (gethash "description" task) "Create PostgreSQL schema with migrations")
     task)
   (let ((task (make-hash-table :test 'equal)))
     (setf (gethash "id" task) "4")
     (setf (gethash "title" task) "Write unit tests")
     (setf (gethash "description" task) "Add Jest tests for components and API")
     (setf (gethash "dependencies" task) '("1" "2"))
     task)))

;;; Test Functions

(defun test-simple-tagging ()
  "Test simple keyword-based tagging."
  (format t "~%=== Testing Simple Tagging ===~%")
  (let ((tagged (paos/tagger::apply-simple-tags *test-tasks*)))
    (dolist (task tagged)
      (format t "Task: ~A~%  Tags: ~{~A~^, ~}~%"
              (gethash "title" task)
              (gethash "tags" task)))
    tagged))

(defun test-grouping ()
  "Test task grouping by tags."
  (format t "~%=== Testing Tag Grouping ===~%")
  (let* ((tagged (paos/tagger::apply-simple-tags *test-tasks*))
         (groups (paos/tagger:group-by-tags tagged)))
    (dolist (group groups)
      (format t "Tag: ~A (~A tasks)~%"
              (getf group :tag)
              (getf group :count)))
    groups))

(defun test-parallel-identification ()
  "Test identification of parallel-safe groups."
  (format t "~%=== Testing Parallel Group Identification ===~%")
  (let* ((tagged (paos/tagger::apply-simple-tags *test-tasks*))
         (parallel-groups (paos/tagger:identify-parallel-groups tagged)))
    (dolist (group parallel-groups)
      (format t "Tag: ~A~%  Parallelizable: ~A~%  Reason: ~A~%  Tasks: ~A~%"
              (getf group :tag)
              (getf group :parallelizable)
              (getf group :reason)
              (length (getf group :tasks))))
    parallel-groups))

(defun test-execution-plan ()
  "Test execution plan generation."
  (format t "~%=== Testing Execution Plan ===~%")
  (let* ((tagged (paos/tagger::apply-simple-tags *test-tasks*))
         (parallel-groups (paos/tagger:identify-parallel-groups tagged))
         (plan (paos/tagger:build-execution-plan parallel-groups)))
    (dolist (wave plan)
      (format t "Wave ~A: ~A groups~%"
              (getf wave :wave)
              (length (getf wave :groups))))
    plan))

(defun test-validation ()
  "Test tag validation."
  (format t "~%=== Testing Validation ===~%")
  (let* ((tagged (paos/tagger::apply-simple-tags *test-tasks*))
         (validation (paos/tagger:validate-tag-groups tagged)))
    (format t "Valid: ~A~%" (getf validation :valid))
    (when (getf validation :issues)
      (format t "Issues:~%")
      (dolist (issue (getf validation :issues))
        (format t "  - ~A~%" issue)))
    (when (getf validation :message)
      (format t "~A~%" (getf validation :message)))
    validation))

(defun test-tag-and-group ()
  "Test high-level tag-and-group API."
  (format t "~%=== Testing Tag-and-Group API ===~%")
  (let ((result (paos/tagger:tag-and-group *test-tasks* :use-ai nil)))
    (format t "Tagged tasks: ~A~%" (length (getf result :tasks)))
    (format t "Groups: ~A~%" (length (getf result :groups)))
    (format t "Execution waves: ~A~%" (length (getf result :plan)))
    (format t "Validation: ~A~%" (getf (getf result :validation) :valid))
    result))

(defun run-tagger-tests ()
  "Run all tagger tests."
  (format t "~%========================================~%")
  (format t "PAOS Intelligent Tagger Test Suite~%")
  (format t "========================================~%")
  (test-simple-tagging)
  (test-grouping)
  (test-parallel-identification)
  (test-execution-plan)
  (test-validation)
  (test-tag-and-group)
  (format t "~%========================================~%")
  (format t "Tests Complete~%")
  (format t "========================================~%"))
