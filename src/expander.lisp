;;;; PAOS - Subtask Expansion
;;;; Decompose high-level tasks into atomic, executable subtasks

(in-package #:paos/expander)

;;; ============================================================================
;;; Subtask Expansion with Claude API
;;; ============================================================================

(defvar *max-subtasks* 8
  "Maximum number of subtasks to generate per task.")

(defvar *min-subtasks* 3
  "Minimum number of subtasks to generate per task.")

(defvar *max-hierarchy-depth* 3
  "Maximum depth of subtask hierarchy (1 = task, 2 = subtask, 3 = sub-subtask).")

(defun expand-task (task &key (depth 1) (max-depth *max-hierarchy-depth*))
  "Expand a task into subtasks using Claude API.
Returns task with populated subtasks array."
  (when (>= depth max-depth)
    (log-info "Max hierarchy depth reached for task ~A" (gethash "title" task))
    (return-from expand-task task))
  
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (prompt (build-expansion-prompt task depth))
             (response (paos/core:call-claude-api api-key prompt))
             (subtasks (parse-expansion-response response)))
        
        ;; Add subtasks to task
        (setf (gethash "subtasks" task) 
              (validate-subtasks subtasks task))
        
        ;; Recursively expand complex subtasks if not at max depth
        (when (< depth (1- max-depth))
          (dolist (subtask (gethash "subtasks" task))
            (when (should-expand-further-p subtask)
              (expand-task subtask :depth (1+ depth) :max-depth max-depth))))
        
        task)
    (error (e)
      (log-error "Task expansion failed for '~A': ~A" 
                (gethash "title" task) e)
      ;; Return task with simple fallback subtasks
      (setf (gethash "subtasks" task)
            (create-fallback-subtasks task))
      task)))

(defun build-expansion-prompt (task depth)
  "Build prompt for Claude to expand task into subtasks."
  (format nil "You are a software engineering expert. Break down this task into ~A-~A atomic, executable subtasks.

Task Details:
Title: ~A
Description: ~A
~@[Priority: ~A~%~]~@[Dependencies: ~{~A~^, ~}~%~]

Each subtask should:
- Be atomic (single responsibility, clearly testable)
- Cover one specific aspect of the parent task
- Have clear completion criteria
- Be implementable by a developer

Return ONLY valid JSON:
{
  \"subtasks\": [
    {
      \"title\": \"Clear, actionable title\",
      \"description\": \"Detailed what and how\",
      \"estimatedComplexity\": \"low|medium|high\",
      \"dependencies\": [\"subtask-id\"],
      \"testStrategy\": \"How to verify completion\",
      \"implementationNotes\": \"Technical guidance\"
    },
    ...
  ],
  \"coverage\": {
    \"complete\": true/false,
    \"gaps\": [\"any missing aspects\"]
  }
}"
          *min-subtasks*
          *max-subtasks*
          (gethash "title" task)
          (gethash "description" task)
          (gethash "priority" task)
          (gethash "dependencies" task)))

(defun parse-expansion-response (response)
  "Parse Claude's subtask expansion response."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response)))
             (text (cdr (assoc :text (car content)))))
        ;; Extract JSON
        (let ((json-start (position #\{ text))
              (json-end (position #\} text :from-end t)))
          (if (and json-start json-end)
              (let ((parsed (cl-json:decode-json-from-string 
                            (subseq text json-start (1+ json-end)))))
                (cdr (assoc :subtasks parsed)))
              (error "No valid JSON in response"))))
    (error (e)
      (log-error "Failed to parse expansion response: ~A" e)
      nil)))

(defun validate-subtasks (subtasks parent-task)
  "Validate and normalize subtasks from Claude response."
  (let ((validated '()))
    (loop for subtask in subtasks
          for id from 1
          do (let ((normalized (make-hash-table :test 'equal)))
               ;; Required fields
               (setf (gethash "id" normalized) (format nil "~A" id))
               (setf (gethash "title" normalized)
                     (or (cdr (assoc :title subtask))
                         (format nil "Subtask ~A" id)))
               (setf (gethash "description" normalized)
                     (or (cdr (assoc :description subtask))
                         (cdr (assoc :title subtask))))
               
               ;; Optional fields
               (setf (gethash "complexity" normalized)
                     (or (cdr (assoc :estimated-complexity subtask)) "medium"))
               (setf (gethash "testStrategy" normalized)
                     (or (cdr (assoc :test-strategy subtask)) ""))
               (setf (gethash "implementationNotes" normalized)
                     (or (cdr (assoc :implementation-notes subtask)) ""))
               (setf (gethash "status" normalized) "pending")
               
               ;; Dependencies
               (let ((deps (cdr (assoc :dependencies subtask))))
                 (when deps
                   (setf (gethash "dependencies" normalized) deps)))
               
               ;; Check atomicity
               (unless (is-atomic-p normalized)
                 (log-warn "Subtask '~A' may not be atomic" 
                          (gethash "title" normalized)))
               
               (push normalized validated)))
    (nreverse validated)))

(defun is-atomic-p (subtask)
  "Check if a subtask is atomic (single responsibility)."
  (let ((title (string-downcase (gethash "title" subtask))))
    ;; Atomic if:
    ;; - Single action verb
    ;; - No "and" conjunctions (usually indicates multiple tasks)
    ;; - Clear, specific scope
    (and (not (search " and " title))
         (not (search " or " title))
         (or (search "implement" title)
             (search "create" title)
             (search "build" title)
             (search "add" title)
             (search "update" title)
             (search "test" title)
             (search "configure" title)))))

(defun should-expand-further-p (subtask)
  "Determine if a subtask should be expanded into sub-subtasks."
  (let ((complexity (gethash "complexity" subtask))
        (description (gethash "description" subtask)))
    (or (string-equal complexity "high")
        (string-equal complexity "very-high")
        (> (length description) 200))))  ; Long description suggests complexity

;;; ============================================================================
;;; Hierarchical Decomposition
;;; ============================================================================

(defun expand-all-tasks (tasks &key (max-depth *max-hierarchy-depth*))
  "Expand all tasks in a list, supporting 2-3 levels of hierarchy."
  (mapcar (lambda (task)
            (expand-task task :depth 1 :max-depth max-depth))
          tasks))

(defun get-expansion-statistics (task)
  "Get statistics about task expansion."
  (let ((stats (make-hash-table :test 'equal)))
     (labels ((count-subtasks (task depth)
                (let ((subtasks (gethash "subtasks" task)))
                  (setf (gethash depth stats)
                        (+ (or (gethash depth stats) 0)
                           (length subtasks)))
                  (dolist (st subtasks)
                    (count-subtasks st (1+ depth))))))
      (count-subtasks task 1))
    stats))

(defun flatten-task-hierarchy (task)
  "Flatten hierarchical task structure into a list.
Useful for iteration and analysis."
  (let ((result (list task)))
    (when (gethash "subtasks" task)
      (dolist (subtask (gethash "subtasks" task))
        (setf result (append result (flatten-task-hierarchy subtask)))))
    result))

;;; ============================================================================
;;; Coverage Validation
;;; ============================================================================

(defun validate-coverage (task)
  "Ensure subtasks fully cover the parent task scope.
Returns validation report."
  (let ((parent-keywords (extract-keywords 
                         (gethash "description" task)))
        (subtask-keywords '())
        (subtasks (gethash "subtasks" task)))
    
    ;; Collect all keywords from subtasks
    (dolist (subtask subtasks)
      (setf subtask-keywords
            (append subtask-keywords
                   (extract-keywords 
                    (gethash "description" subtask)))))
    
    ;; Check coverage
    (let ((covered (intersection parent-keywords subtask-keywords 
                                :test #'string-equal))
          (missing (set-difference parent-keywords subtask-keywords
                                  :test #'string-equal)))
      (list :complete (null missing)
            :coverage-percentage (* 100 (/ (length covered) 
                                          (max 1 (length parent-keywords))))
            :missing-aspects missing
            :subtask-count (length subtasks)))))

(defun extract-keywords (text)
  "Extract key technical terms from text."
  (let ((words (str:words (string-downcase text)))
        (keywords '()))
    (dolist (word words)
      ;; Keep technical terms, actions, and significant nouns
      (when (or (> (length word) 5)
                (member word '("api" "ui" "db" "test" "auth" "data")
                        :test #'string-equal))
        (push word keywords)))
    (remove-duplicates keywords :test #'string-equal)))

(defun analyze-task-complexity (task)
  "Analyze task complexity to determine appropriate subtask count."
  (let* ((description (gethash "description" task))
         (word-count (length (str:words description)))
         (has-multiple-concerns (or (search " and " description)
                                   (search "including" description)))
         (technical-terms (extract-keywords description)))
    
    (list :estimated-subtasks
          (cond
            ((> word-count 100) *max-subtasks*)
            ((< word-count 30) *min-subtasks*)
            (t (ceiling (/ word-count 15))))
          :complexity
          (cond
            ((> (length technical-terms) 8) "very-high")
            ((> (length technical-terms) 5) "high")
            ((> (length technical-terms) 3) "medium")
            (t "low"))
          :concerns (if has-multiple-concerns "multiple" "single"))))

;;; ============================================================================
;;; Fallback Subtask Generation
;;; ============================================================================

(defun create-fallback-subtasks (task)
  "Create simple fallback subtasks when API fails."
  (let ((description (gethash "description" task))
        (subtasks '()))
    
    ;; Basic 3-step breakdown
    (push (make-subtask "1" "Design and Planning"
                       (format nil "Design approach for: ~A" 
                              (gethash "title" task)))
          subtasks)
    (push (make-subtask "2" "Core Implementation"
                       (format nil "Implement: ~A" description))
          subtasks)
    (push (make-subtask "3" "Testing and Validation"
                       (format nil "Test and validate: ~A"
                              (gethash "title" task)))
          subtasks)
    
    (nreverse subtasks)))

(defun make-subtask (id title description)
  "Create a subtask hash table."
  (let ((subtask (make-hash-table :test 'equal)))
    (setf (gethash "id" subtask) id)
    (setf (gethash "title" subtask) title)
    (setf (gethash "description" subtask) description)
    (setf (gethash "status" subtask) "pending")
    (setf (gethash "complexity" subtask) "medium")
    subtask))

;;; ============================================================================
;;; Error Handling
;;; ============================================================================

(defun expand-task-with-retry (task &key (max-retries 3))
  "Expand task with retry logic."
  (loop for attempt from 1 to max-retries
        do (handler-case
               (return (expand-task task))
             (error (e)
               (log-warn "Expansion attempt ~A failed: ~A" attempt e)
               (when (= attempt max-retries)
                 (log-error "All expansion attempts failed, using fallback")
                 (setf (gethash "subtasks" task)
                       (create-fallback-subtasks task))
                 (return task))))
        finally (return task)))

(defun validate-expanded-task (task)
  "Validate that task expansion is complete and correct."
  (let ((issues '()))
    
    ;; Check has subtasks
    (unless (gethash "subtasks" task)
      (push "No subtasks generated" issues))
    
    ;; Check subtask count
    (let ((count (length (gethash "subtasks" task))))
      (when (< count *min-subtasks*)
        (push (format nil "Too few subtasks: ~A" count) issues))
      (when (> count *max-subtasks*)
        (push (format nil "Too many subtasks: ~A" count) issues)))
    
    ;; Check coverage
    (let ((coverage (validate-coverage task)))
      (when (< (getf coverage :coverage-percentage) 70)
        (push (format nil "Low coverage: ~A%" 
                     (getf coverage :coverage-percentage))
              issues)))
    
    ;; Check atomicity
    (dolist (subtask (gethash "subtasks" task))
      (unless (is-atomic-p subtask)
        (push (format nil "Non-atomic subtask: ~A"
                     (gethash "title" subtask))
              issues)))
    
    (if issues
        (list :valid nil :issues issues)
        (list :valid t))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================
;;; Note: get-api-key and call-claude-api now provided by paos/core

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
