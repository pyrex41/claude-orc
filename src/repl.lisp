;;;; PAOS - Human-in-the-Loop REPL
;;;; Interactive command interface for orchestrator control

(in-package #:paos/human-interface)

;;; ============================================================================
;;; REPL State
;;; ============================================================================

(defvar *repl-running* nil
  "Flag indicating if REPL is active.")

(defvar *repl-history* '()
  "Command history for REPL.")

(defvar *orchestrator-state* nil
  "Reference to orchestrator state.")

(defclass repl-context ()
  ((orchestrator :initarg :orchestrator :accessor repl-orchestrator)
   (dashboard :initarg :dashboard :accessor repl-dashboard)
   (agents :initarg :agents :accessor repl-agents :initform '())
   (current-tab :accessor repl-current-tab :initform nil)))

;;; ============================================================================
;;; Core REPL Loop
;;; ============================================================================

(defun start-repl (&key orchestrator dashboard)
  "Start the interactive REPL loop."
  (let ((context (make-instance 'repl-context
                               :orchestrator orchestrator
                               :dashboard dashboard)))
    (setf *repl-running* t)
    (setf *orchestrator-state* context)
    
    (format t "~%~A~%"
            (paos/dashboard::colorize 
             "PAOS Interactive Console" 
             paos/dashboard::*ansi-cyan* :bold t))
    (format t "Type 'help' for commands, 'quit' to exit~%~%")
    
    (unwind-protect
         (repl-loop context)
      (cleanup-repl context))))

(defun repl-loop (context)
  "Main REPL loop - read, evaluate, print."
  (loop while *repl-running*
        do (handler-case
               (progn
                 (format t "paos> ")
                 (force-output)
                 (let ((input (read-line *standard-input* nil :eof)))
                   (cond
                     ((eq input :eof)
                      (setf *repl-running* nil))
                     ((not (str:empty? input))
                      (push input *repl-history*)
                      (process-command context input)))))
             (error (e)
               (format t "~AError: ~A~A~%"
                      paos/dashboard::*ansi-red*
                      e
                      paos/dashboard::*ansi-reset*)))))

(defun cleanup-repl (context)
  "Cleanup REPL resources."
  (declare (ignore context))
  (format t "~%Exiting PAOS console...~%")
  (setf *repl-running* nil))

;;; ============================================================================
;;; Command Processing
;;; ============================================================================

(defun process-command (context input)
  "Parse and execute a command."
  (let* ((tokens (str:words input))
         (command (string-downcase (car tokens)))
         (args (cdr tokens)))
    
    (cond
      ;; Core commands
      ((string= command "help") (cmd-help args))
      ((string= command "quit") (cmd-quit args))
      ((string= command "exit") (cmd-quit args))
      
      ;; Status commands
      ((string= command "status") (cmd-status context args))
      ((string= command "agents") (cmd-agents context args))
      ((string= command "tasks") (cmd-tasks context args))
      
      ;; Agent control
      ((string= command "pause") (cmd-pause context args))
      ((string= command "resume") (cmd-resume context args))
      ((string= command "switch") (cmd-switch context args))
      ((string= command "chat") (cmd-chat context args))
      
      ;; Dashboard control
      ((string= command "refresh") (cmd-refresh context args))
      ((string= command "dashboard") (cmd-dashboard context args))
      
      ;; Feedback
      ((string= command "feedback") (cmd-feedback context args))
      ((string= command "review") (cmd-review context args))
      
      ;; Unknown command
      (t (format t "Unknown command: ~A. Type 'help' for available commands.~%"
                command)))))

;;; ============================================================================
;;; Core Commands
;;; ============================================================================

(defun cmd-help (&optional args)
  "Display help information."
  (declare (ignore args))
  (format t "~%~A~%~%"
          (paos/dashboard::colorize "Available Commands:" 
                                   paos/dashboard::*ansi-cyan* :bold t))
  
  (format t "~A~%"
          (paos/dashboard::colorize "Core:" paos/dashboard::*ansi-yellow* :bold t))
  (format t "  help                 - Show this help~%")
  (format t "  quit, exit           - Exit REPL~%~%")
  
  (format t "~A~%"
          (paos/dashboard::colorize "Status:" paos/dashboard::*ansi-yellow* :bold t))
  (format t "  status               - Show orchestrator status~%")
  (format t "  agents               - List all agents~%")
  (format t "  tasks [agent]        - Show tasks (optionally for specific agent)~%~%")
  
  (format t "~A~%"
          (paos/dashboard::colorize "Agent Control:" paos/dashboard::*ansi-yellow* :bold t))
  (format t "  pause <agent>        - Pause an agent~%")
  (format t "  resume <agent>       - Resume a paused agent~%")
  (format t "  switch <agent>       - Switch to agent's Zellij tab~%")
  (format t "  chat <agent> <msg>   - Send message to agent~%~%")
  
  (format t "~A~%"
          (paos/dashboard::colorize "Dashboard:" paos/dashboard::*ansi-yellow* :bold t))
  (format t "  refresh              - Refresh dashboard~%")
  (format t "  dashboard [on|off]   - Toggle dashboard~%~%")
  
  (format t "~A~%"
          (paos/dashboard::colorize "Feedback:" paos/dashboard::*ansi-yellow* :bold t))
  (format t "  feedback <agent> <msg> - Provide feedback to agent~%")
  (format t "  review <agent>       - Review agent's work~%~%"))

(defun cmd-quit (&optional args)
  "Quit the REPL."
  (declare (ignore args))
  (setf *repl-running* nil))

;;; ============================================================================
;;; Status Commands
;;; ============================================================================

(defun cmd-status (context &optional args)
  "Show orchestrator status."
  (declare (ignore args))
  (format t "~%~A~%"
          (paos/dashboard::colorize "Orchestrator Status:" 
                                   paos/dashboard::*ansi-cyan* :bold t))
  
  (let ((agents (repl-agents context)))
    (format t "Active agents: ~A~%" (length agents))
    (format t "Current tab: ~A~%" (or (repl-current-tab context) "none"))
    (format t "REPL history: ~A commands~%~%" (length *repl-history*))))

(defun cmd-agents (context &optional args)
  "List all agents."
  (declare (ignore args))
  (format t "~%~A~%~%"
          (paos/dashboard::colorize "Active Agents:" 
                                   paos/dashboard::*ansi-cyan* :bold t))
  
  (let ((agents (repl-agents context)))
    (if agents
        (dolist (agent agents)
          (format t "  ~A ~A~%"
                  (paos/dashboard::render-status-indicator 
                   (getf agent :status))
                  (getf agent :tag)))
        (format t "  No active agents~%"))
    (format t "~%")))

(defun cmd-tasks (context &optional args)
  "Show tasks, optionally filtered by agent."
  (let ((agent-tag (car args)))
    (format t "~%~A~%~%"
            (paos/dashboard::colorize 
             (if agent-tag
                 (format nil "Tasks for ~A:" agent-tag)
                 "All Tasks:")
             paos/dashboard::*ansi-cyan* :bold t))
    
    ;; This would integrate with actual task system
    (format t "  [Task listing would appear here]~%~%")))

;;; ============================================================================
;;; Agent Control Commands
;;; ============================================================================

(defun cmd-pause (context args)
  "Pause an agent."
  (let ((agent-tag (car args)))
    (if agent-tag
        (progn
          (format t "Pausing agent: ~A~%" agent-tag)
          ;; Send pause signal to agent
          (send-agent-command context agent-tag "pause"))
        (format t "~AUsage: pause <agent>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

(defun cmd-resume (context args)
  "Resume a paused agent."
  (let ((agent-tag (car args)))
    (if agent-tag
        (progn
          (format t "Resuming agent: ~A~%" agent-tag)
          (send-agent-command context agent-tag "resume"))
        (format t "~AUsage: resume <agent>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

(defun cmd-switch (context args)
  "Switch to an agent's Zellij tab."
  (let ((agent-tag (car args)))
    (if agent-tag
        (progn
          (format t "Switching to agent: ~A~%" agent-tag)
          (paos/zellij:switch-zellij-tab agent-tag)
          (setf (repl-current-tab context) agent-tag))
        (format t "~AUsage: switch <agent>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

(defun cmd-chat (context args)
  "Send a message to an agent."
  (let ((agent-tag (car args))
        (message (str:join " " (cdr args))))
    (if (and agent-tag (not (str:empty? message)))
        (progn
          (format t "Sending to ~A: ~A~%" agent-tag message)
          (send-agent-message context agent-tag message))
        (format t "~AUsage: chat <agent> <message>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

;;; ============================================================================
;;; Dashboard Commands
;;; ============================================================================

(defun cmd-refresh (context &optional args)
  "Refresh the dashboard."
  (declare (ignore args))
  (when (repl-dashboard context)
    (format t "Refreshing dashboard...~%")
    ;; Trigger dashboard refresh
    (paos/dashboard:render-dashboard (repl-dashboard context))))

(defun cmd-dashboard (context args)
  "Toggle dashboard on/off."
  (let ((action (car args)))
    (cond
      ((string= action "on")
       (format t "Starting dashboard...~%")
       ;; Start dashboard if not running
       )
      ((string= action "off")
       (format t "Stopping dashboard...~%")
       (paos/dashboard:stop-dashboard))
      (t
       (format t "~AUsage: dashboard [on|off]~A~%"
               paos/dashboard::*ansi-yellow*
               paos/dashboard::*ansi-reset*)))))

;;; ============================================================================
;;; Feedback Commands
;;; ============================================================================

(defun cmd-feedback (context args)
  "Provide feedback to an agent."
  (let ((agent-tag (car args))
        (feedback (str:join " " (cdr args))))
    (if (and agent-tag (not (str:empty? feedback)))
        (progn
          (format t "~ASending feedback to ~A~A~%"
                  paos/dashboard::*ansi-green*
                  agent-tag
                  paos/dashboard::*ansi-reset*)
          (send-agent-feedback context agent-tag feedback))
        (format t "~AUsage: feedback <agent> <message>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

(defun cmd-review (context args)
  "Review an agent's work."
  (let ((agent-tag (car args)))
    (if agent-tag
        (progn
          (format t "~AReviewing work from: ~A~A~%"
                  paos/dashboard::*ansi-cyan*
                  agent-tag
                  paos/dashboard::*ansi-reset*)
          (show-agent-review context agent-tag))
        (format t "~AUsage: review <agent>~A~%"
                paos/dashboard::*ansi-yellow*
                paos/dashboard::*ansi-reset*))))

;;; ============================================================================
;;; Agent Communication
;;; ============================================================================

(defun send-agent-command (context agent-tag command)
  "Send a command to a specific agent."
  (let ((agent (find-agent context agent-tag)))
    (if agent
        (let ((tab-name (getf agent :tab-name)))
          (paos/zellij:send-to-tab 
           (or (getf agent :session) "paos-orchestrator")
           tab-name
           command)
          (format t "~ACommand sent~A~%"
                  paos/dashboard::*ansi-green*
                  paos/dashboard::*ansi-reset*))
        (format t "~AAgent not found: ~A~A~%"
                paos/dashboard::*ansi-red*
                agent-tag
                paos/dashboard::*ansi-reset*))))

(defun send-agent-message (context agent-tag message)
  "Send a chat message to an agent."
  (send-agent-command context agent-tag 
                     (format nil "# ORCHESTRATOR: ~A" message)))

(defun send-agent-feedback (context agent-tag feedback)
  "Send feedback to an agent."
  (send-agent-command context agent-tag
                     (format nil "# FEEDBACK: ~A" feedback)))

(defun show-agent-review (context agent-tag)
  "Show review information for an agent."
  (let ((agent (find-agent context agent-tag)))
    (if agent
        (progn
          (format t "~%Progress: ~A%~%" (or (getf agent :progress) 0))
          (format t "Status: ~A~%" (getf agent :status))
          (format t "Worktree: ~A~%~%" (getf agent :worktree)))
        (format t "~AAgent not found: ~A~A~%"
                paos/dashboard::*ansi-red*
                agent-tag
                paos/dashboard::*ansi-reset*))))

;;; ============================================================================
;;; Input Parsing & Validation
;;; ============================================================================

(defun parse-command-line (input)
  "Parse command line input into command and arguments."
  (let ((trimmed (str:trim input)))
    (if (str:empty? trimmed)
        (values nil nil)
        (let ((tokens (str:words trimmed)))
          (values (string-downcase (car tokens))
                 (cdr tokens))))))

(defun validate-agent-tag (context tag)
  "Validate that an agent tag exists."
  (find-agent context tag))

(defun find-agent (context tag)
  "Find an agent by tag."
  (find tag (repl-agents context)
        :key (lambda (a) (getf a :tag))
        :test #'string-equal))

;;; ============================================================================
;;; History Management
;;; ============================================================================

(defun show-history (&optional (n 10))
  "Show last N commands from history."
  (format t "~%Recent commands:~%")
  (loop for cmd in (subseq *repl-history* 0 (min n (length *repl-history*)))
        for i from 1
        do (format t "  ~A. ~A~%" i cmd))
  (format t "~%"))

(defun clear-history ()
  "Clear command history."
  (setf *repl-history* '())
  (format t "History cleared~%"))

;;; ============================================================================
;;; Integration with Orchestrator
;;; ============================================================================

(defun update-repl-agents (context agents)
  "Update REPL's knowledge of active agents."
  (setf (repl-agents context) agents))

(defun get-orchestrator-state ()
  "Get current orchestrator state."
  *orchestrator-state*)

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun repl-prompt (context)
  "Generate REPL prompt string."
  (let ((tab (repl-current-tab context)))
    (if tab
        (format nil "paos[~A]> " tab)
        "paos> ")))
