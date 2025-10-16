;;;; PAOS - Direct Agent Communication
;;;; Tab switching and command protocol for agent interaction

(in-package #:paos/zellij)

;;; ============================================================================
;;; Enhanced Tab Switching (Task 15.1)
;;; ============================================================================

(defun focus-agent (tag &key session-name)
  "Focus on an agent's Zellij tab for direct interaction."
  (switch-zellij-tab tag :session-name session-name))

(defun switch-to-agent-tab (agent-info)
  "Switch to agent's tab using agent info structure."
  (let ((tab-name (getf agent-info :tab-name))
        (session (getf agent-info :session)))
    (switch-zellij-tab tab-name :session-name session)))

;;; ============================================================================
;;; Agent Command Protocol (Task 15.2)
;;; ============================================================================

(defvar *agent-commands* (make-hash-table :test 'equal)
  "Registry of commands agents can respond to.")

(defclass agent-command ()
  ((name :initarg :name :accessor command-name)
   (description :initarg :description :accessor command-description)
   (handler :initarg :handler :accessor command-handler)))

(defun register-agent-command (name description handler)
  "Register a command that agents can respond to."
  (setf (gethash name *agent-commands*)
        (make-instance 'agent-command
                      :name name
                      :description description
                      :handler handler)))

;; Register standard commands
(register-agent-command 
 "pause"
 "Pause agent execution"
 (lambda (agent) 
   (format nil "# SYSTEM: PAUSE - Stop current work and await instructions")))

(register-agent-command
 "resume"
 "Resume agent execution"
 (lambda (agent)
   (format nil "# SYSTEM: RESUME - Continue work from last checkpoint")))

(register-agent-command
 "status"
 "Request status update"
 (lambda (agent)
   (format nil "# SYSTEM: STATUS - Report current progress and status")))

(register-agent-command
 "checkpoint"
 "Create checkpoint"
 (lambda (agent)
   (format nil "# SYSTEM: CHECKPOINT - Save current state and report progress")))

(defun send-command-to-agent (agent command-name &rest args)
  "Send a command to an agent."
  (let ((cmd (gethash command-name *agent-commands*)))
    (if cmd
        (let ((message (funcall (command-handler cmd) agent)))
          (send-to-tab (getf agent :session)
                      (getf agent :tab-name)
                      message)
          (log-info "Sent '~A' to agent ~A" command-name (getf agent :tag)))
        (log-error "Unknown command: ~A" command-name))))

;;; ============================================================================
;;; Agent State Management
;;; ============================================================================

(defvar *agent-states* (make-hash-table :test 'equal)
  "Track agent states for command responses.")

(defclass agent-state ()
  ((tag :initarg :tag :accessor agent-tag)
   (paused :accessor agent-paused :initform nil)
   (checkpoint :accessor agent-checkpoint :initform nil)
   (last-command :accessor agent-last-command :initform nil)
   (last-response :accessor agent-last-response :initform nil)))

(defun get-agent-state (tag)
  "Get or create agent state."
  (or (gethash tag *agent-states*)
      (setf (gethash tag *agent-states*)
            (make-instance 'agent-state :tag tag))))

(defun update-agent-state (tag &key paused checkpoint command response)
  "Update agent state after command."
  (let ((state (get-agent-state tag)))
    (when paused
      (setf (agent-paused state) paused))
    (when checkpoint
      (setf (agent-checkpoint state) checkpoint))
    (when command
      (setf (agent-last-command state) command))
    (when response
      (setf (agent-last-response state) response))
    state))
