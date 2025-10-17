;;;; PAOS - Intelligent Task Tagging
;;;; Domain-specific tagging for parallel execution grouping

(in-package #:paos/tagger)

;;; ============================================================================
;;; Task Tagging with Claude API
;;; ============================================================================

(defvar *common-tags* '("ui" "backend" "database" "api" "infrastructure" 
                        "testing" "documentation" "security" "devops" "frontend")
  "Common domain tags for task categorization.")

(defun tag-tasks (tasks)
  "Assign domain-specific tags to tasks using Claude API.
Returns tasks with added 'tags' field."
  (handler-case
      (let* ((api-key (get-api-key))
             (prompt (build-tagging-prompt tasks))
             (response (call-claude-api api-key prompt))
             (tagged-data (parse-tagging-response response)))
        (apply-tags-to-tasks tasks tagged-data))
    (error (e)
      (log-error "Task tagging failed: ~A" e)
      ;; Fallback: use simple keyword-based tagging
      (apply-simple-tags tasks))))

(defun build-tagging-prompt (tasks)
  "Build prompt for Claude to assign tags to tasks."
  (format nil "You are a software architecture expert. Analyze these tasks and assign domain-specific tags.

Available tags: ~{~A~^, ~}

Tasks:
~A

For each task, assign 1-3 tags that best describe its domain/category. Consider:
- Technical domain (frontend, backend, database, api, etc.)
- Work type (implementation, testing, documentation, etc.)
- Infrastructure needs (devops, security, infrastructure, etc.)

Return ONLY valid JSON:
{
  \"taggedTasks\": [
    {\"id\": \"task-id\", \"tags\": [\"tag1\", \"tag2\"]},
    ...
  ],
  \"tagGroups\": {
    \"tag-name\": {
      \"description\": \"What this group does\",
      \"canRunParallel\": true/false,
      \"dependencies\": [\"other-tag\"]
    }
  }
}"
          *common-tags*
          (format-tasks-for-prompt tasks)))

(defun format-tasks-for-prompt (tasks)
  "Format tasks into readable text for the prompt."
  (with-output-to-string (stream)
    (dolist (task tasks)
      (format stream "~%ID: ~A~%Title: ~A~%Description: ~A~%~%"
              (or (gethash "id" task) (gethash "title" task))
              (gethash "title" task)
              (gethash "description" task)))))

(defun parse-tagging-response (response)
  "Parse Claude's tagging response."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response)))
             (text (cdr (assoc :text (car content)))))
        ;; Extract JSON from response
        (let ((json-start (position #\{ text))
              (json-end (position #\} text :from-end t)))
          (if (and json-start json-end)
              (cl-json:decode-json-from-string 
               (subseq text json-start (1+ json-end)))
              (error "No valid JSON in response"))))
    (error (e)
      (log-error "Failed to parse tagging response: ~A" e)
      nil)))

(defun apply-tags-to-tasks (tasks tagged-data)
  "Apply tags from Claude response to task structures."
  (let ((tag-map (make-tag-map (cdr (assoc :tagged-tasks tagged-data)))))
    (mapcar (lambda (task)
              (let ((task-id (or (gethash "id" task) 
                                (gethash "title" task))))
                (when task-id
                  (let ((tags (gethash task-id tag-map)))
                    (when tags
                      (setf (gethash "tags" task) tags)))))
              task)
            tasks)))

(defun make-tag-map (tagged-tasks)
  "Create hash map of task-id -> tags from Claude response."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (item tagged-tasks)
      (let ((id (cdr (assoc :id item)))
            (tags (cdr (assoc :tags item))))
        (when (and id tags)
          (setf (gethash id map) tags))))
    map))

;;; ============================================================================
;;; Simple Fallback Tagging
;;; ============================================================================

(defun apply-simple-tags (tasks)
  "Apply simple keyword-based tags as fallback."
  (mapcar (lambda (task)
            (let ((tags (detect-tags-from-text 
                        (gethash "title" task)
                        (gethash "description" task))))
              (setf (gethash "tags" task) tags))
            task)
          tasks))

(defun detect-tags-from-text (title description)
  "Detect tags from task text using keyword matching."
  (let ((text (string-downcase 
               (concatenate 'string title " " description)))
        (tags '()))
    
    ;; Frontend/UI detection
    (when (or (search "ui" text) (search "frontend" text)
              (search "react" text) (search "component" text)
              (search "css" text) (search "html" text))
      (push "ui" tags))
    
    ;; Backend detection
    (when (or (search "backend" text) (search "server" text)
              (search "api" text) (search "endpoint" text)
              (search "service" text))
      (push "backend" tags))
    
    ;; Database detection
    (when (or (search "database" text) (search "db" text)
              (search "sql" text) (search "schema" text)
              (search "migration" text) (search "query" text))
      (push "database" tags))
    
    ;; API detection
    (when (or (search "api" text) (search "rest" text)
              (search "graphql" text) (search "endpoint" text))
      (push "api" tags))
    
    ;; Testing detection
    (when (or (search "test" text) (search "testing" text)
              (search "unit" text) (search "integration" text))
      (push "testing" tags))
    
    ;; Security detection
    (when (or (search "security" text) (search "auth" text)
              (search "encrypt" text) (search "permission" text))
      (push "security" tags))
    
    ;; DevOps/Infrastructure
    (when (or (search "deploy" text) (search "ci/cd" text)
              (search "docker" text) (search "kubernetes" text)
              (search "infrastructure" text))
      (push "devops" tags))
    
    ;; Documentation
    (when (or (search "document" text) (search "readme" text)
              (search "docs" text) (search "comment" text))
      (push "documentation" tags))
    
    ;; Default tag if nothing matched
    (unless tags
      (push "general" tags))
    
    (remove-duplicates tags :test #'string-equal)))

;;; ============================================================================
;;; Parallel Execution Grouping
;;; ============================================================================

(defun group-by-tags (tasks)
  "Group tasks by their tags for parallel execution."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let ((task-tags (gethash "tags" task)))
        (when task-tags
          (dolist (tag task-tags)
            (push task (gethash tag groups '()))))))
    
    ;; Convert to list of groups
    (let ((result '()))
      (maphash (lambda (tag task-list)
                 (push (list :tag tag 
                           :tasks (nreverse task-list)
                           :count (length task-list))
                       result))
               groups)
      (sort result #'> :key (lambda (g) (getf g :count))))))

(defun identify-parallel-groups (tasks)
  "Identify groups of tasks that can run in parallel.
Returns list of parallel-safe task groups."
  (let* ((tagged-tasks (ensure-tags tasks))
         (tag-groups (group-by-tags tagged-tasks))
         (parallel-groups '()))
    
    ;; Analyze each tag group for parallelizability
    (dolist (group tag-groups)
      (let* ((tag (getf group :tag))
             (group-tasks (getf group :tasks))
             (can-parallel (can-run-parallel-p group-tasks)))
        (push (list :tag tag
                   :tasks group-tasks
                   :parallelizable can-parallel
                   :reason (if can-parallel
                              "No inter-task dependencies detected"
                              "Tasks have dependencies"))
              parallel-groups)))
    
    (nreverse parallel-groups)))

(defun ensure-tags (tasks)
  "Ensure all tasks have tags, applying simple tags if missing."
  (mapcar (lambda (task)
            (unless (gethash "tags" task)
              (let ((tags (detect-tags-from-text 
                          (gethash "title" task)
                          (gethash "description" task))))
                (setf (gethash "tags" task) tags)))
            task)
          tasks))

(defun can-run-parallel-p (tasks)
  "Check if a group of tasks can run in parallel.
Returns T if no dependencies exist between tasks in the group."
  (let ((task-ids (mapcar (lambda (t) (gethash "id" t)) tasks)))
    (every (lambda (task)
             (let ((deps (gethash "dependencies" task)))
               (or (null deps)
                   (not (intersection deps task-ids :test #'equal)))))
           tasks)))

(defun analyze-dependencies (tasks)
  "Analyze dependencies between tasks to identify execution order."
  (let ((dependency-graph (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let ((task-id (gethash "id" task))
            (deps (gethash "dependencies" task)))
        (setf (gethash task-id dependency-graph) 
              (or deps '()))))
    dependency-graph))

(defun build-execution-plan (parallel-groups)
  "Build execution plan from parallel groups.
Returns ordered list of execution waves."
  (let ((waves '())
        (remaining-groups (copy-list parallel-groups)))
    
    ;; First wave: groups with no dependencies
    (let ((no-deps (remove-if-not 
                    (lambda (g) (getf g :parallelizable))
                    remaining-groups)))
      (when no-deps
        (push (list :wave 1 :groups no-deps) waves)
        (setf remaining-groups 
              (set-difference remaining-groups no-deps))))
    
    ;; Subsequent waves: process dependent groups
    (loop for wave-num from 2
          while remaining-groups
          do (let ((wave-groups '()))
               ;; Add groups whose dependencies are satisfied
               (dolist (group remaining-groups)
                 (when (dependencies-satisfied-p group waves)
                   (push group wave-groups)))
               
               (when wave-groups
                 (push (list :wave wave-num :groups wave-groups) waves)
                 (setf remaining-groups
                       (set-difference remaining-groups wave-groups)))
               
               ;; Safety: break if stuck
               (when (and remaining-groups (null wave-groups))
                 (push (list :wave wave-num 
                           :groups remaining-groups
                           :warning "Circular dependencies detected")
                       waves)
                 (setf remaining-groups nil))))
    
    (nreverse waves)))

(defun dependencies-satisfied-p (group completed-waves)
  "Check if a group's dependencies are satisfied by completed waves."
  ;; Simplified: assume satisfied if not in first wave
  ;; Full implementation would track actual task completion
  t)

;;; ============================================================================
;;; Validation
;;; ============================================================================

(defun validate-tag-groups (tasks)
  "Validate that tags are properly assigned and groups are safe."
  (let ((issues '()))
    
    ;; Check all tasks have tags
    (dolist (task tasks)
      (unless (gethash "tags" task)
        (push (format nil "Task '~A' has no tags" 
                     (gethash "title" task))
              issues)))
    
    ;; Check for conflicting parallel assignments
    (let ((parallel-groups (identify-parallel-groups tasks)))
      (dolist (group parallel-groups)
        (let ((group-tasks (getf group :tasks)))
          (when (and (getf group :parallelizable)
                    (has-file-conflicts-p group-tasks))
            (push (format nil "Tag '~A' group has potential file conflicts"
                         (getf group :tag))
                  issues)))))
    
    ;; Return validation result
    (if issues
        (list :valid nil :issues issues)
        (list :valid t :message "All tags valid"))))

(defun has-file-conflicts-p (tasks)
  "Check if tasks might conflict on file modifications.
Simplified heuristic based on task descriptions."
  ;; Look for similar file/component mentions
  (let ((file-mentions '()))
    (dolist (task tasks)
      (let ((desc (string-downcase (gethash "description" task))))
        ;; Extract potential file/component names
        (dolist (word (str:words desc))
          (when (or (str:ends-with? ".js" word)
                    (str:ends-with? ".lisp" word)
                    (str:ends-with? ".ts" word)
                    (str:contains? "/" word))
            (push word file-mentions)))))
    
    ;; Check for duplicates
    (< (length (remove-duplicates file-mentions :test #'string-equal))
       (length file-mentions))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun get-api-key ()
  "Get Anthropic API key."
  (or (uiop:getenv "ANTHROPIC_API_KEY")
      (error "ANTHROPIC_API_KEY not found")))

(defun call-claude-api (api-key prompt)
  "Call Claude API (shared implementation)."
  (let* ((headers `(("x-api-key" . ,api-key)
                   ("anthropic-version" . "2023-06-01")
                   ("content-type" . "application/json")))
         (body (cl-json:encode-json-to-string
                `(("model" . "claude-3-5-sonnet-20241022")
                  ("max_tokens" . 4096)
                  ("messages" . ((("role" . "user")
                                 ("content" . ,prompt)))))))
         (response (dexador:post "https://api.anthropic.com/v1/messages"
                                :headers headers
                                :content body)))
    response))

(defun log-error (format-string &rest args)
  "Log error message."
  (format *error-output* "ERROR: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))

;;; ============================================================================
;;; High-level API
;;; ============================================================================

(defun tag-and-group (tasks &key use-ai)
  "Tag tasks and create parallel execution groups.
If use-ai is T, uses Claude API. Otherwise uses simple keyword matching."
  (let* ((tagged-tasks (if use-ai
                          (tag-tasks tasks)
                          (apply-simple-tags tasks)))
         (parallel-groups (identify-parallel-groups tagged-tasks))
         (execution-plan (build-execution-plan parallel-groups))
         (validation (validate-tag-groups tagged-tasks)))
    
    (list :tasks tagged-tasks
          :groups parallel-groups
          :plan execution-plan
          :validation validation)))
