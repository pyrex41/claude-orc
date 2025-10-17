;;;; PAOS - Agent-Computer Interface (ACI)
;;;; Tool definitions for agents following Anthropic best practices

(in-package #:paos/aci)

;;; ============================================================================
;;; Tool Schema Definitions
;;; ============================================================================

(defvar *tool-registry* (make-hash-table :test 'equal)
  "Registry of all available tools for agents.")

(defclass tool-definition ()
  ((name :initarg :name :accessor tool-name :type string)
   (description :initarg :description :accessor tool-description :type string)
   (parameters :initarg :parameters :accessor tool-parameters :type hash-table)
   (handler :initarg :handler :accessor tool-handler :type function)
   (category :initarg :category :accessor tool-category :initform "general")
   (safe :initarg :safe :accessor tool-safe :initform t)))

(defun define-tool (name description parameters handler &key category (safe t))
  "Define a new tool for agents.
Parameters should be a hash table with JSON schema format."
  (let ((tool (make-instance 'tool-definition
                            :name name
                            :description description
                            :parameters parameters
                            :handler handler
                            :category category
                            :safe safe)))
    (setf (gethash name *tool-registry*) tool)
    tool))

;;; ============================================================================
;;; Tool Execution Handlers
;;; ============================================================================
;;; These must be defined before the tool definitions that reference them

(defun execute-git-status (args)
  "Execute git status command."
  (let ((path (gethash "worktree_path" args)))
    (handler-case
        (uiop:run-program (list "git" "-C" path "status" "--short")
                         :output :string)
      (error (e)
        (format nil "Error: ~A" e)))))

(defun execute-git-diff (args)
  "Execute git diff command."
  (let ((path (gethash "worktree_path" args))
        (staged (gethash "staged" args)))
    (handler-case
        (uiop:run-program
         (if staged
             (list "git" "-C" path "diff" "--cached")
             (list "git" "-C" path "diff"))
         :output :string)
      (error (e)
        (format nil "Error: ~A" e)))))

(defun execute-git-commit (args)
  "Execute git commit command."
  (let ((path (gethash "worktree_path" args))
        (message (gethash "message" args))
        (files (gethash "files" args)))
    (handler-case
        (let ((cmd (append (list "git" "-C" path "commit" "-m" message)
                          (when files (cons "--" files)))))
          (uiop:run-program cmd :output :string))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun execute-file-read (args)
  "Execute file read."
  (let ((path (gethash "path" args)))
    (handler-case
        (with-open-file (stream path)
          (let ((contents (make-string (file-length stream))))
            (read-sequence contents stream)
            contents))
      (error (e)
        (format nil "Error reading file: ~A" e)))))

(defun execute-file-write (args)
  "Execute file write."
  (let ((path (gethash "path" args))
        (content (gethash "content" args)))
    (handler-case
        (progn
          (ensure-directories-exist (directory-namestring path))
          (with-open-file (stream path
                                 :direction :output
                                 :if-exists :supersede)
            (write-sequence content stream))
          (format nil "Successfully wrote to ~A" path))
      (error (e)
        (format nil "Error writing file: ~A" e)))))

(defun execute-list-directory (args)
  "Execute directory listing."
  (let ((path (gethash "path" args)))
    (handler-case
        (let ((entries (uiop:directory-files path)))
          (format nil "~{~A~%~}" (mapcar #'namestring entries)))
      (error (e)
        (format nil "Error listing directory: ~A" e)))))

(defun execute-update-task-status (args)
  "Execute task status update."
  (let ((task-id (gethash "task_id" args))
        (status (gethash "status" args))
        (notes (gethash "notes" args)))
    (format nil "Task ~A updated to ~A~@[ (~A)~]" task-id status notes)))

(defun execute-request-human-input (args)
  "Execute human input request."
  (let ((question (gethash "question" args))
        (context (gethash "context" args)))
    (format nil "Human input requested: ~A~@[ Context: ~A~]" question context)))

(defun execute-run-command (args)
  "Execute shell command (with safety checks)."
  (let ((command (gethash "command" args))
        (path (gethash "worktree_path" args)))
    (if (command-safe-p command)
        (handler-case
            (uiop:run-program command
                            :output :string
                            :directory path)
          (error (e)
            (format nil "Error: ~A" e)))
        (format nil "Error: Command not whitelisted: ~A" command))))

;;; ============================================================================
;;; Core Tool Definitions
;;; ============================================================================

;; Git Tools
(define-tool "git_status"
  "Get the current git status of the worktree, including modified, staged, and untracked files."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params) 
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "worktree_path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the git worktree")
                    prop))
            props))
    (setf (gethash "required" params) '("worktree_path"))
    params)
  #'execute-git-status
  :category "git")

(define-tool "git_diff"
  "Get the diff of changes in the worktree, showing what has been modified."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "worktree_path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the git worktree")
                    prop))
            (setf (gethash "staged" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "boolean")
                    (setf (gethash "description" prop) "Show staged changes only")
                    prop))
            props))
    (setf (gethash "required" params) '("worktree_path"))
    params)
  #'execute-git-diff
  :category "git")

(define-tool "git_commit"
  "Commit changes to the git repository with a message."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "worktree_path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the git worktree")
                    prop))
            (setf (gethash "message" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Commit message following conventional commits format")
                    prop))
            (setf (gethash "files" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "array")
                    (setf (gethash "items" prop)
                          (let ((items (make-hash-table :test 'equal)))
                            (setf (gethash "type" items) "string")
                            items))
                    (setf (gethash "description" prop) "Specific files to commit (optional)")
                    prop))
            props))
    (setf (gethash "required" params) '("worktree_path" "message"))
    params)
  #'execute-git-commit
  :category "git")

;; File System Tools
(define-tool "file_read"
  "Read the contents of a file from the filesystem."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the file to read")
                    prop))
            props))
    (setf (gethash "required" params) '("path"))
    params)
  #'execute-file-read
  :category "filesystem")

(define-tool "file_write"
  "Write content to a file, creating it if it doesn't exist."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the file to write")
                    prop))
            (setf (gethash "content" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Content to write to the file")
                    prop))
            props))
    (setf (gethash "required" params) '("path" "content"))
    params)
  #'execute-file-write
  :category "filesystem"
  :safe nil)  ; Writing files is not safe by default

(define-tool "list_directory"
  "List files and directories in a given path."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the directory")
                    prop))
            props))
    (setf (gethash "required" params) '("path"))
    params)
  #'execute-list-directory
  :category "filesystem")

;; Task Management Tools
(define-tool "update_task_status"
  "Update the status of a task in the orchestrator."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "task_id" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "ID of the task to update")
                    prop))
            (setf (gethash "status" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "enum" prop) '("pending" "in-progress" "done" "blocked"))
                    (setf (gethash "description" prop) "New status for the task")
                    prop))
            (setf (gethash "notes" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Optional notes about the status change")
                    prop))
            props))
    (setf (gethash "required" params) '("task_id" "status"))
    params)
  #'execute-update-task-status
  :category "task")

(define-tool "request_human_input"
  "Request input or clarification from a human operator."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "question" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Question or request for the human")
                    prop))
            (setf (gethash "context" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Context about why input is needed")
                    prop))
            props))
    (setf (gethash "required" params) '("question"))
    params)
  #'execute-request-human-input
  :category "human-interaction")

;; Command Execution (Restricted)
(define-tool "run_command"
  "Execute a shell command in the worktree (restricted to safe commands)."
  (let ((params (make-hash-table :test 'equal)))
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params)
          (let ((props (make-hash-table :test 'equal)))
            (setf (gethash "command" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Command to execute (must be whitelisted)")
                    prop))
            (setf (gethash "worktree_path" props)
                  (let ((prop (make-hash-table :test 'equal)))
                    (setf (gethash "type" prop) "string")
                    (setf (gethash "description" prop) "Path to the worktree")
                    prop))
            props))
    (setf (gethash "required" params) '("command" "worktree_path"))
    params)
  #'execute-run-command
  :category "execution"
  :safe nil)

;;; ============================================================================
;;; Tool Validation
;;; ============================================================================

(defun validate-tool-schema (tool)
  "Validate a tool definition against Anthropic's schema requirements."
  (let ((issues '()))
    
    ;; Check required fields
    (unless (tool-name tool)
      (push "Missing tool name" issues))
    (unless (tool-description tool)
      (push "Missing tool description" issues))
    (unless (tool-parameters tool)
      (push "Missing parameters definition" issues))
    (unless (tool-handler tool)
      (push "Missing handler function" issues))
    
    ;; Validate description length
    (when (and (tool-description tool)
               (< (length (tool-description tool)) 10))
      (push "Description too short (min 10 chars)" issues))
    
    ;; Validate parameters schema
    (when (tool-parameters tool)
      (unless (string= (gethash "type" (tool-parameters tool)) "object")
        (push "Parameters type must be 'object'" issues))
      (unless (gethash "properties" (tool-parameters tool))
        (push "Parameters must have 'properties' field" issues)))
    
    (if issues
        (list :valid nil :issues issues)
        (list :valid t))))

(defun validate-tool-arguments (tool-name args)
  "Validate arguments against tool schema."
  (let ((tool (gethash tool-name *tool-registry*)))
    (unless tool
      (return-from validate-tool-arguments 
        (list :valid nil :error "Tool not found")))
    
    (let* ((params (tool-parameters tool))
           (required (gethash "required" params))
           (properties (gethash "properties" params))
           (issues '()))
      
      ;; Check required parameters
      (dolist (req-param required)
        (unless (gethash req-param args)
          (push (format nil "Missing required parameter: ~A" req-param) issues)))
      
      ;; Validate parameter types
      (maphash (lambda (key value)
                 (let ((prop-def (gethash key properties)))
                   (when prop-def
                     (let ((expected-type (gethash "type" prop-def)))
                       (unless (validate-type value expected-type)
                         (push (format nil "Invalid type for ~A: expected ~A" 
                                     key expected-type)
                               issues))))))
               args)
      
      (if issues
          (list :valid nil :issues issues)
          (list :valid t)))))

(defun validate-type (value expected-type)
  "Validate a value matches expected JSON schema type."
  (cond
    ((string= expected-type "string") (stringp value))
    ((string= expected-type "number") (numberp value))
    ((string= expected-type "boolean") (or (eq value t) (eq value nil)))
    ((string= expected-type "array") (listp value))
    ((string= expected-type "object") (hash-table-p value))
    (t t)))

;;; ============================================================================
;;; Safety and Whitelisting
;;; ============================================================================

(defvar *safe-commands* '("npm" "make" "test" "build" "lint")
  "Whitelist of safe commands that can be executed.")

(defun command-safe-p (command)
  "Check if a command is whitelisted for execution."
  (let ((cmd-name (car (str:words command))))
    (member cmd-name *safe-commands* :test #'string-equal)))

(defun get-safe-tools ()
  "Get list of all safe tools."
  (let ((safe-tools '()))
    (maphash (lambda (name tool)
               (when (tool-safe tool)
                 (push name safe-tools)))
             *tool-registry*)
    safe-tools))

;;; ============================================================================
;;; Tool Registry Management
;;; ============================================================================

(defun get-tool (name)
  "Get a tool definition by name."
  (gethash name *tool-registry*))

(defun list-tools (&key category)
  "List all available tools, optionally filtered by category."
  (let ((tools '()))
    (maphash (lambda (name tool)
               (when (or (null category)
                        (string= (tool-category tool) category))
                 (push (list :name name
                           :description (tool-description tool)
                           :category (tool-category tool)
                           :safe (tool-safe tool))
                       tools)))
             *tool-registry*)
    tools))

(defun tool-schemas ()
  "Export all tool schemas in Anthropic format."
  (let ((schemas '()))
    (maphash (lambda (name tool)
               (push (list :name name
                         :description (tool-description tool)
                         :input_schema (tool-parameters tool))
                     schemas))
             *tool-registry*)
    schemas))

(defun execute-tool (name args)
  "Execute a tool with given arguments."
  (let ((tool (get-tool name)))
    (unless tool
      (error "Tool not found: ~A" name))
    
    ;; Validate arguments
    (let ((validation (validate-tool-arguments name args)))
      (unless (getf validation :valid)
        (error "Invalid arguments: ~A" (getf validation :issues))))
    
    ;; Execute handler
    (funcall (tool-handler tool) args)))

;;; ============================================================================
;;; Claude Code Integration
;;; ============================================================================

(defun generate-tools-config ()
  "Generate tools configuration for Claude Code."
  (let ((config (make-hash-table :test 'equal)))
    (setf (gethash "tools" config) (tool-schemas))
    (setf (gethash "safe_tools" config) (get-safe-tools))
    config))

(defun export-tools-json (output-file)
  "Export tools to JSON file for Claude Code."
  (with-open-file (stream output-file
                         :direction :output
                         :if-exists :supersede)
    (write-string (cl-json:encode-json-to-string 
                   (generate-tools-config))
                 stream)))
