;;;; PAOS - Zellij Integration
;;;; Terminal multiplexer integration for agent spawning

(in-package #:paos/zellij)

;;; ============================================================================
;;; Zellij Tab Management
;;; ============================================================================

(defvar *zellij-command* "zellij"
  "Command to invoke Zellij.")

(defvar *default-session-name* "paos-orchestrator"
  "Default Zellij session name for PAOS.")

(defun create-zellij-tab (tag &key session-name layout)
  "Create a new Zellij tab for an agent group.
Returns the tab name or NIL on failure."
  (handler-case
      (let* ((tab-name (format-tab-name tag))
             (session (or session-name *default-session-name*))
             (result (run-zellij-action session "new-tab" 
                                       :name tab-name
                                       :layout layout)))
        (when result
          (log-info "Created Zellij tab: ~A" tab-name)
          tab-name))
    (error (e)
      (log-error "Failed to create Zellij tab for ~A: ~A" tag e)
      nil)))

(defun switch-zellij-tab (tab-name &key session-name)
  "Switch to a specific Zellij tab."
  (handler-case
      (let ((session (or session-name *default-session-name*)))
        (run-zellij-action session "go-to-tab-name" :name tab-name))
    (error (e)
      (log-error "Failed to switch to tab ~A: ~A" tab-name e)
      nil)))

(defun close-zellij-tab (tab-name &key session-name)
  "Close a Zellij tab."
  (handler-case
      (let ((session (or session-name *default-session-name*)))
        (run-zellij-action session "close-tab" :name tab-name))
    (error (e)
      (log-error "Failed to close tab ~A: ~A" tab-name e)
      nil)))

(defun list-zellij-tabs (&key session-name)
  "List all tabs in the Zellij session.
Returns list of tab names."
  (handler-case
      (let* ((session (or session-name *default-session-name*))
             (output (run-zellij-command 
                     (list "action" "query-tab-names" 
                          "--session" session))))
        (parse-tab-list output))
    (error (e)
      (log-error "Failed to list Zellij tabs: ~A" e)
      '())))

;;; ============================================================================
;;; Agent Spawning
;;; ============================================================================

(defun spawn-agent (tag worktree-path context-file &key session-name)
  "Spawn a Claude Code agent in a new Zellij tab.
Returns spawned agent info or NIL on failure."
  (handler-case
      (let* ((tab-name (create-zellij-tab tag :session-name session-name))
             (session (or session-name *default-session-name*)))
        (when tab-name
          ;; Send commands to the new tab
          (send-to-tab session tab-name 
                      (format nil "cd ~A" worktree-path))
          (sleep 0.5)  ; Allow cd to complete
          
          ;; Start Claude Code with context
          (let ((claude-cmd (build-claude-command context-file)))
            (send-to-tab session tab-name claude-cmd)
            
            ;; Return agent info
            (list :tag tag
                  :tab-name tab-name
                  :worktree worktree-path
                  :context context-file
                  :status "initializing"
                  :session session))))
    (error (e)
      (log-error "Failed to spawn agent for ~A: ~A" tag e)
      nil)))

(defun build-claude-command (context-file)
  "Build the command to start Claude Code with context."
  (if context-file
      (format nil "claude --context ~A" context-file)
      "claude"))

(defun send-to-tab (session tab-name command)
  "Send a command to a specific Zellij tab."
  (run-zellij-action session "write-chars" 
                    :tab tab-name
                    :chars (format nil "~A~C" command #\Newline)))

;;; ============================================================================
;;; Session Management
;;; ============================================================================

(defun create-zellij-session (&key session-name layout)
  "Create a new Zellij session for PAOS."
  (handler-case
      (let ((session (or session-name *default-session-name*)))
        (if (session-exists-p session)
            (progn
              (log-info "Zellij session ~A already exists" session)
              session)
            (let ((args (list "action" "new-session" session)))
              (when layout
                (setf args (append args (list "--layout" layout))))
              (run-zellij-command args)
              (log-info "Created Zellij session: ~A" session)
              session)))
    (error (e)
      (log-error "Failed to create Zellij session: ~A" e)
      nil)))

(defun attach-to-session (&key session-name)
  "Attach to existing Zellij session."
  (let ((session (or session-name *default-session-name*)))
    (if (session-exists-p session)
        (run-zellij-command (list "attach" session))
        (log-error "Session ~A does not exist" session))))

(defun session-exists-p (session-name)
  "Check if a Zellij session exists."
  (handler-case
      (let ((output (run-zellij-command (list "list-sessions"))))
        (search session-name output))
    (error () nil)))

(defun get-session-info (&key session-name)
  "Get information about current Zellij session."
  (handler-case
      (let* ((session (or session-name *default-session-name*))
             (output (run-zellij-command 
                     (list "action" "dump-layout" 
                          "--session" session))))
        (parse-session-layout output))
    (error (e)
      (log-error "Failed to get session info: ~A" e)
      nil)))

;;; ============================================================================
;;; Low-level Zellij Commands
;;; ============================================================================

(defun run-zellij-command (args)
  "Execute a Zellij command with arguments.
Returns command output as string."
  (handler-case
      (let ((full-args (cons *zellij-command* args)))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program full-args
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
          (if (zerop exit-code)
              output
              (progn
                (log-error "Zellij command failed (exit ~A): ~A" 
                          exit-code error-output)
                nil))))
    (error (e)
      (log-error "Failed to run Zellij command: ~A" e)
      nil)))

(defun run-zellij-action (session action &rest params)
  "Run a Zellij action in a session.
Params are keyword-value pairs that get converted to CLI args."
  (let ((args (list "action" action "--session" session)))
    ;; Add parameters
    (loop for (key value) on params by #'cddr
          do (let ((param-name (string-downcase (symbol-name key))))
               (setf args (append args (list (format nil "--~A" param-name)
                                           (format nil "~A" value))))))
    (run-zellij-command args)))

;;; ============================================================================
;;; Context File Management
;;; ============================================================================

(defun create-agent-context (tag tasks worktree-path)
  "Create a context file for an agent with its assigned tasks.
Returns context file path or NIL on failure."
  (handler-case
      (let ((context-file (format nil "~A/.paos/context-~A.md"
                                 worktree-path tag)))
        (ensure-directories-exist (directory-namestring context-file))

        (with-open-file (stream context-file
                               :direction :output
                               :if-exists :supersede)
          (format stream "# PAOS Agent Context: ~A~%~%" tag)
          (format stream "## Assigned Tasks~%~%")
          (dolist (task tasks)
            (format stream "### ~A~%~%" (gethash "title" task))
            (format stream "**Description:** ~A~%~%" (gethash "description" task))
            (when (gethash "tags" task)
              (format stream "**Tags:** ~{~A~^, ~}~%~%" (gethash "tags" task)))
            (when (gethash "subtasks" task)
              (format stream "~%**Subtasks:**~%")
              (dolist (subtask (gethash "subtasks" task))
                (format stream "- ~A~%" (gethash "title" subtask))))
            (format stream "~%---~%~%")))

        (log-info "Created agent context: ~A" context-file)
        context-file)
    (error (e)
      (log-error "Failed to create context file for ~A: ~A" tag e)
      nil)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun format-tab-name (tag)
  "Format a tag into a valid Zellij tab name."
  (let ((name (string-downcase tag)))
    ;; Replace invalid characters
    (setf name (ppcre:regex-replace-all "[^a-z0-9-]" name "-"))
    ;; Limit length
    (if (> (length name) 20)
        (subseq name 0 20)
        name)))

(defun parse-tab-list (output)
  "Parse Zellij tab list output into list of tab names."
  (when output
    (let ((lines (str:lines output)))
      (remove-if #'str:empty? 
                (mapcar #'str:trim lines)))))

(defun parse-session-layout (output)
  "Parse Zellij session layout output."
  ;; Simplified parsing - full implementation would parse YAML/JSON
  (when output
    (list :raw output
          :tab-count (count #\Newline output))))

(defun check-zellij-installed ()
  "Check if Zellij is installed and accessible."
  (handler-case
      (let ((version (run-zellij-command '("--version"))))
        (when version
          (log-info "Zellij detected: ~A" (str:trim version))
          t))
    (error ()
      (log-error "Zellij not found. Please install: https://zellij.dev")
      nil)))

;;; ============================================================================
;;; Error Handling
;;; ============================================================================

(defun spawn-agent-with-retry (tag worktree-path context-file 
                               &key (max-retries 3) session-name)
  "Spawn agent with retry logic."
  (loop for attempt from 1 to max-retries
        do (handler-case
               (let ((result (spawn-agent tag worktree-path context-file
                                        :session-name session-name)))
                 (when result
                   (return result)))
             (error (e)
               (log-warn "Agent spawn attempt ~A failed: ~A" attempt e)
               (when (= attempt max-retries)
                 (log-error "All spawn attempts failed for ~A" tag)
                 (return nil))))))

(defun validate-spawn-result (spawn-info)
  "Validate that agent spawned successfully."
  (and spawn-info
       (getf spawn-info :tab-name)
       (getf spawn-info :worktree)
       (string-equal (getf spawn-info :status) "initializing")))

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
