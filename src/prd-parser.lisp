;;;; PAOS - PRD Parser
;;;; Multi-format PRD parsing (Markdown, Plain Text, YAML)

(in-package #:paos/prd-parser)

;;; ============================================================================
;;; Markdown PRD Parser
;;; ============================================================================

(defun parse-markdown-prd (file-path)
  "Parse a Markdown-formatted PRD file into structured task data.
Returns a hash table with extracted tasks, sections, and metadata."
  (handler-case
      (let ((content (read-file-to-string file-path)))
        (extract-markdown-structure content))
    (error (e)
      (log-error "Failed to parse Markdown PRD ~A: ~A" file-path e)
      nil)))

(defun extract-markdown-structure (content)
  "Extract structured data from Markdown content."
  (let ((result (make-hash-table :test 'equal))
        (sections (extract-markdown-sections content)))
    (setf (gethash "format" result) "markdown")
    (setf (gethash "sections" result) sections)
    (setf (gethash "tasks" result) (extract-tasks-from-sections sections))
    (setf (gethash "metadata" result) (extract-markdown-metadata sections))
    result))

(defun extract-markdown-sections (content)
  "Extract sections from Markdown content based on headers."
  (let ((sections '())
        (current-section nil)
        (current-content ""))
    (dolist (line (str:lines content))
      (cond
        ;; Header line (## or #)
        ((ppcre:scan "^#{1,3}\\s+(.+)$" line)
         (when current-section
           (push (list :title current-section
                      :content (str:trim current-content))
                 sections))
         (setf current-section (ppcre:regex-replace "^#{1,3}\\s+" line ""))
         (setf current-content ""))
        ;; Regular content line
        (t
         (setf current-content (concatenate 'string current-content line (string #\Newline))))))
    ;; Add last section
    (when current-section
      (push (list :title current-section
                 :content (str:trim current-content))
            sections))
    (nreverse sections)))

(defun extract-tasks-from-sections (sections)
  "Extract task items from sections, looking for lists and task indicators."
  (let ((tasks '()))
    (dolist (section sections)
      (let ((content (getf section :content)))
        ;; Look for numbered lists or bullet points
        (dolist (line (str:lines content))
          (when (or (ppcre:scan "^\\d+\\.\\s+(.+)$" line)  ; 1. Task
                    (ppcre:scan "^[-*]\\s+(.+)$" line))    ; - Task or * Task
            (let ((task-text (extract-task-text line)))
              (when (and task-text (task-line-p task-text))
                (push (make-task-from-text task-text (getf section :title))
                      tasks)))))))
    (nreverse tasks)))

(defun extract-task-text (line)
  "Extract the task text from a list item line."
  (or (nth-value 1 (ppcre:scan-to-strings "^\\d+\\.\\s+(.+)$" line))
      (nth-value 1 (ppcre:scan-to-strings "^[-*]\\s+(.+)$" line))))

(defun task-line-p (text)
  "Determine if a line represents a task (not just any list item)."
  (and text
       (> (length text) 10)  ; Minimum length
       (or (ppcre:scan "(?i)\\b(implement|create|build|add|develop|set up|configure)\\b" text)
           (ppcre:scan "(?i)\\b(should|must|need to|will)\\b" text))))

(defun make-task-from-text (text section-title)
  "Create a task structure from extracted text and section title."
  (let ((task (make-hash-table :test 'equal)))
    (setf (gethash "title" task) text)
    (setf (gethash "description" task) text)
    (setf (gethash "section" task) section-title)
    (setf (gethash "priority" task) "medium")
    (setf (gethash "status" task) "pending")
    task))

(defun extract-markdown-metadata (sections)
  "Extract metadata from sections like 'Overview', 'Goals', etc."
  (let ((metadata (make-hash-table :test 'equal)))
    (dolist (section sections)
      (let ((title (string-downcase (getf section :title))))
        (cond
          ((or (str:contains? "overview" title)
               (str:contains? "summary" title))
           (setf (gethash "overview" metadata) (getf section :content)))
          ((or (str:contains? "goal" title)
               (str:contains? "objective" title))
           (setf (gethash "goals" metadata) (getf section :content)))
          ((or (str:contains? "requirement" title)
               (str:contains? "spec" title))
           (setf (gethash "requirements" metadata) (getf section :content))))))
    metadata))

;;; ============================================================================
;;; Plain Text PRD Parser
;;; ============================================================================

(defun parse-plain-text-prd (file-path)
  "Parse a plain text PRD file into structured task data.
Uses pattern matching to identify tasks, descriptions, and dependencies."
  (handler-case
      (let ((content (read-file-to-string file-path)))
        (extract-plain-text-structure content))
    (error (e)
      (log-error "Failed to parse plain text PRD ~A: ~A" file-path e)
      nil)))

(defun extract-plain-text-structure (content)
  "Extract structured data from plain text content."
  (let ((result (make-hash-table :test 'equal))
        (lines (str:lines content))
        (tasks '())
        (metadata (make-hash-table :test 'equal)))
    
    ;; Extract overview/summary from first paragraph
    (let ((first-para (find-first-paragraph lines)))
      (when first-para
        (setf (gethash "overview" metadata) first-para)))
    
    ;; Extract tasks from lines
    (dolist (line lines)
      (when (task-indicator-p line)
        (push (make-task-from-text 
               (clean-task-text line)
               "plain-text-section")
              tasks)))
    
    (setf (gethash "format" result) "plain-text")
    (setf (gethash "tasks" result) (nreverse tasks))
    (setf (gethash "metadata" result) metadata)
    result))

(defun find-first-paragraph (lines)
  "Find the first substantial paragraph in the text."
  (let ((para "")
        (started nil))
    (dolist (line lines)
      (let ((trimmed (str:trim line)))
        (cond
          ((and (not started) (> (length trimmed) 20))
           (setf started t)
           (setf para trimmed))
          ((and started (> (length trimmed) 0))
           (setf para (concatenate 'string para " " trimmed)))
          ((and started (= (length trimmed) 0))
           (return-from find-first-paragraph para)))))
    para))

(defun task-indicator-p (line)
  "Check if a line indicates a task based on patterns."
  (and (> (length line) 15)
       (or
        ;; Numbered items
        (ppcre:scan "^\\s*\\d+[\\.\\)]" line)
        ;; Action words at start
        (ppcre:scan "(?i)^\\s*(implement|create|build|add|develop|set up|configure|ensure|provide)" line)
        ;; Modal verbs indicating requirements
        (ppcre:scan "(?i)\\b(should|must|need to|will|shall)\\s+(implement|create|build|add)" line)
        ;; Task list indicators
        (ppcre:scan "^\\s*[-*•]\\s+[A-Z]" line))))

(defun clean-task-text (line)
  "Clean task text by removing list markers and extra whitespace."
  (let ((cleaned line))
    ;; Remove list markers
    (setf cleaned (ppcre:regex-replace "^\\s*\\d+[\\.\\)]\\s*" cleaned ""))
    (setf cleaned (ppcre:regex-replace "^\\s*[-*•]\\s*" cleaned ""))
    ;; Trim and return
    (str:trim cleaned)))

;;; ============================================================================
;;; YAML PRD Parser
;;; ============================================================================

(defun parse-yaml-prd (file-path)
  "Parse a YAML-formatted PRD file into structured task data.
Directly deserializes YAML structure into task information."
  (handler-case
      (let ((yaml-data (cl-yaml:parse (read-file-to-string file-path))))
        (convert-yaml-to-tasks yaml-data file-path))
    (error (e)
      (log-error "Failed to parse YAML PRD ~A: ~A" file-path e)
      nil)))

(defun convert-yaml-to-tasks (yaml-data file-path)
  "Convert YAML data structure to task format."
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "format" result) "yaml")
    
    ;; Handle different YAML structures
    (cond
      ;; Direct task list
      ((gethash "tasks" yaml-data)
       (setf (gethash "tasks" result) 
             (mapcar #'normalize-yaml-task (gethash "tasks" yaml-data))))
      
      ;; Requirements that need to be converted to tasks
      ((gethash "requirements" yaml-data)
       (setf (gethash "tasks" result)
             (requirements-to-tasks (gethash "requirements" yaml-data))))
      
      ;; Features to be converted
      ((gethash "features" yaml-data)
       (setf (gethash "tasks" result)
             (features-to-tasks (gethash "features" yaml-data))))
      
      ;; Unknown structure - try to extract tasks
      (t
       (setf (gethash "tasks" result)
             (extract-tasks-from-yaml yaml-data))))
    
    ;; Extract metadata
    (setf (gethash "metadata" result) (extract-yaml-metadata yaml-data))
    result))

(defun normalize-yaml-task (yaml-task)
  "Normalize a YAML task to standard task format."
  (let ((task (make-hash-table :test 'equal)))
    (setf (gethash "title" task) 
          (or (gethash "title" yaml-task)
              (gethash "name" yaml-task)
              "Untitled Task"))
    (setf (gethash "description" task)
          (or (gethash "description" yaml-task)
              (gethash "desc" yaml-task)
              (gethash "title" task)))
    (setf (gethash "priority" task)
          (or (gethash "priority" yaml-task) "medium"))
    (setf (gethash "status" task)
          (or (gethash "status" yaml-task) "pending"))
    (when (gethash "dependencies" yaml-task)
      (setf (gethash "dependencies" task)
            (gethash "dependencies" yaml-task)))
    task))

(defun requirements-to-tasks (requirements)
  "Convert requirements list to tasks."
  (if (listp requirements)
      (mapcar (lambda (req)
                (let ((task (make-hash-table :test 'equal)))
                  (setf (gethash "title" task) (if (hash-table-p req)
                                                   (gethash "title" req)
                                                   req))
                  (setf (gethash "description" task) (gethash "title" task))
                  (setf (gethash "priority" task) "medium")
                  (setf (gethash "status" task) "pending")
                  task))
              requirements)
      '()))

(defun features-to-tasks (features)
  "Convert features list to tasks."
  (requirements-to-tasks features))  ; Same conversion logic

(defun extract-tasks-from-yaml (yaml-data)
  "Try to extract tasks from unknown YAML structure."
  (let ((tasks '()))
    (maphash (lambda (key value)
               (when (and (stringp key)
                         (or (str:contains? "task" (string-downcase key))
                             (str:contains? "feature" (string-downcase key))
                             (str:contains? "requirement" (string-downcase key))))
                 (if (listp value)
                     (setf tasks (append tasks (requirements-to-tasks value)))
                     (push (make-task-from-text value key) tasks))))
             yaml-data)
    tasks))

(defun extract-yaml-metadata (yaml-data)
  "Extract metadata fields from YAML data."
  (let ((metadata (make-hash-table :test 'equal)))
    (dolist (key '("overview" "summary" "description" "goals" "objectives"))
      (when (gethash key yaml-data)
        (setf (gethash key metadata) (gethash key yaml-data))))
    metadata))

;;; ============================================================================
;;; Common Utilities
;;; ============================================================================

(defun read-file-to-string (file-path)
  "Read entire file contents into a string."
  (with-open-file (stream file-path)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun detect-prd-format (file-path)
  "Detect the format of a PRD file based on extension and content."
  (let ((extension (pathname-type file-path)))
    (cond
      ((string-equal extension "md") :markdown)
      ((string-equal extension "yaml") :yaml)
      ((string-equal extension "yml") :yaml)
      ((string-equal extension "txt") :plain-text)
      (t
       ;; Try to detect from content
       (handler-case
           (let ((content (read-file-to-string file-path)))
             (cond
               ((ppcre:scan "^#{1,3}\\s+" content) :markdown)
               ((ppcre:scan "^\\w+:\\s*$" content) :yaml)
               (t :plain-text)))
         (error () :plain-text))))))

(defun parse-prd (file-path)
  "Parse a PRD file automatically detecting its format."
  (let ((format (detect-prd-format file-path)))
    (case format
      (:markdown (parse-markdown-prd file-path))
      (:yaml (parse-yaml-prd file-path))
      (:plain-text (parse-plain-text-prd file-path))
      (t (parse-plain-text-prd file-path)))))  ; Default to plain text

(defun extract-tasks (parsed-prd)
  "Extract the task list from parsed PRD data."
  (when (hash-table-p parsed-prd)
    (gethash "tasks" parsed-prd)))

;;; ============================================================================
;;; Logging (from decomposer.lisp pattern)
;;; ============================================================================

(defun log-error (format-string &rest args)
  "Log an error message."
  (format *error-output* "ERROR: ~A~%" (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log an info message."
  (format t "INFO: ~A~%" (apply #'format nil format-string args)))
