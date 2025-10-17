;;;; PAOS - Checkpoint System
;;;; Automatic pause points for human review

(in-package #:paos/checkpoints)

;;; ============================================================================
;;; Checkpoint Configuration (Task 17.1)
;;; ============================================================================

(defvar *checkpoint-config* nil
  "Checkpoint configuration loaded from config file.")

(defclass checkpoint-config ()
  ((pre-commit :initarg :pre-commit :accessor checkpoint-pre-commit :initform t)
   (at-blockers :initarg :at-blockers :accessor checkpoint-at-blockers :initform t)
   (after-subtask :initarg :after-subtask :accessor checkpoint-after-subtask :initform nil)
   (on-error :initarg :on-error :accessor checkpoint-on-error :initform t)
   (manual-only :initarg :manual-only :accessor checkpoint-manual-only :initform nil)))

(defun load-checkpoint-config (config-hash)
  "Load checkpoint configuration from config hash."
  (let ((checkpoint-section (gethash "checkpoints" config-hash)))
    (setf *checkpoint-config*
          (make-instance 'checkpoint-config
                        :pre-commit (or (gethash "pre_commit" checkpoint-section) t)
                        :at-blockers (or (gethash "at_blockers" checkpoint-section) t)
                        :after-subtask (gethash "after_subtask" checkpoint-section)
                        :on-error (or (gethash "on_error" checkpoint-section) t)
                        :manual-only (gethash "manual_only" checkpoint-section)))
    *checkpoint-config*))

(defun checkpoint-p (checkpoint-type)
  "Check if a checkpoint type is enabled."
  (unless *checkpoint-config*
    (setf *checkpoint-config* (make-instance 'checkpoint-config)))
  
  (case checkpoint-type
    (:pre-commit (checkpoint-pre-commit *checkpoint-config*))
    (:at-blockers (checkpoint-at-blockers *checkpoint-config*))
    (:after-subtask (checkpoint-after-subtask *checkpoint-config*))
    (:on-error (checkpoint-on-error *checkpoint-config*))
    (:manual (checkpoint-manual-only *checkpoint-config*))
    (t nil)))

;;; ============================================================================
;;; Checkpoint Execution (Task 17.2)
;;; ============================================================================

(defclass checkpoint ()
  ((id :initarg :id :accessor checkpoint-id)
   (type :initarg :type :accessor checkpoint-type)
   (agent-tag :initarg :agent-tag :accessor checkpoint-agent)
   (context :initarg :context :accessor checkpoint-context)
   (timestamp :initarg :timestamp :accessor checkpoint-timestamp)
   (approved :accessor checkpoint-approved :initform nil)))

(defvar *active-checkpoints* (make-hash-table :test 'equal)
  "Active checkpoints awaiting approval.")

(defun create-checkpoint (agent-tag type context)
  "Create a new checkpoint."
  (let ((checkpoint (make-instance 'checkpoint
                                  :id (generate-checkpoint-id)
                                  :type type
                                  :agent-tag agent-tag
                                  :context context
                                  :timestamp (get-universal-time))))
    (setf (gethash (checkpoint-id checkpoint) *active-checkpoints*)
          checkpoint)
    checkpoint))

(defun generate-checkpoint-id ()
  "Generate unique checkpoint ID."
  (format nil "cp-~A-~A" 
          (get-universal-time)
          (random 10000)))

(defun pause-at-checkpoint (agent-tag checkpoint-type context)
  "Pause agent execution at a checkpoint.
Returns checkpoint object."
  (when (checkpoint-p checkpoint-type)
    (log-info "Checkpoint triggered for ~A: ~A" agent-tag checkpoint-type)
    
    (let ((checkpoint (create-checkpoint agent-tag checkpoint-type context)))
      
      ;; Notify orchestrator
      (notify-checkpoint checkpoint)

      ;; TODO: Implement agent state management in zellij integration
      ;; Update agent state
      ;; (paos/zellij:update-agent-state agent-tag
      ;;                                :paused t
      ;;                                :checkpoint checkpoint)

      ;; TODO: Implement command sending in zellij integration
      ;; Send pause command to agent
      ;; (paos/zellij:send-command-to-agent
      ;;  (list :tag agent-tag :session "paos-orchestrator" :tab-name agent-tag)
      ;;  "pause")

      checkpoint)))

(defun notify-checkpoint (checkpoint)
  "Notify user of checkpoint."
  (format t "~%~A🛑 CHECKPOINT: ~A~A~%"
          paos/dashboard::*ansi-yellow*
          (checkpoint-agent checkpoint)
          paos/dashboard::*ansi-reset*)
  (format t "Type: ~A~%" (checkpoint-type checkpoint))
  (format t "Context: ~A~%"(checkpoint-context checkpoint))
  (format t "~%Awaiting approval. Use 'approve ~A' to continue.~%~%"
          (checkpoint-id checkpoint)))

;;; ============================================================================
;;; Resume Logic (Task 17.3)
;;; ============================================================================

(defun approve-checkpoint (checkpoint-id)
  "Approve a checkpoint and resume agent."
  (let ((checkpoint (gethash checkpoint-id *active-checkpoints*)))
    (if checkpoint
        (progn
          (setf (checkpoint-approved checkpoint) t)
          (resume-from-checkpoint checkpoint)
          (remhash checkpoint-id *active-checkpoints*)
          (log-info "Checkpoint ~A approved" checkpoint-id)
          t)
        (progn
          (log-error "Checkpoint not found: ~A" checkpoint-id)
          nil))))

(defun resume-from-checkpoint (checkpoint)
  "Resume agent execution from checkpoint."
  (let ((agent-tag (checkpoint-agent checkpoint)))

    ;; TODO: Implement agent state management in zellij integration
    ;; Update agent state
    ;; (paos/zellij:update-agent-state agent-tag
    ;;                                :paused nil
    ;;                                :checkpoint nil)

    ;; TODO: Implement command sending in zellij integration
    ;; Send resume command
    ;; (paos/zellij:send-command-to-agent
    ;;  (list :tag agent-tag :session "paos-orchestrator" :tab-name agent-tag)
    ;;  "resume")

    (log-info "Resumed agent ~A from checkpoint" agent-tag)))

(defun reject-checkpoint (checkpoint-id reason)
  "Reject a checkpoint with reason."
  (let ((checkpoint (gethash checkpoint-id *active-checkpoints*)))
    (when checkpoint
      (log-info "Checkpoint ~A rejected: ~A" checkpoint-id reason)
      (remhash checkpoint-id *active-checkpoints*)
      ;; Could implement alternative actions here
      t)))

(defun list-active-checkpoints ()
  "List all active checkpoints."
  (let ((checkpoints '()))
    (maphash (lambda (id checkpoint)
               (push (list :id id
                          :agent (checkpoint-agent checkpoint)
                          :type (checkpoint-type checkpoint)
                          :timestamp (checkpoint-timestamp checkpoint))
                     checkpoints))
             *active-checkpoints*)
    (nreverse checkpoints)))

;;; ============================================================================
;;; Checkpoint Types
;;; ============================================================================

(defun checkpoint-before-commit (agent-tag files)
  "Checkpoint before git commit."
  (pause-at-checkpoint agent-tag :pre-commit
                      (format nil "About to commit ~A files" (length files))))

(defun checkpoint-on-blocker (agent-tag blocker-description)
  "Checkpoint when agent hits blocker."
  (pause-at-checkpoint agent-tag :at-blockers blocker-description))

(defun checkpoint-after-subtask (agent-tag subtask)
  "Checkpoint after completing subtask."
  (when (checkpoint-p :after-subtask)
    (pause-at-checkpoint agent-tag :after-subtask
                        (format nil "Completed: ~A" 
                               (gethash "title" subtask)))))

(defun checkpoint-on-error (agent-tag error-message)
  "Checkpoint when error occurs."
  (pause-at-checkpoint agent-tag :on-error error-message))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun log-error (format-string &rest args)
  "Log error message."
  (format *error-output* "ERROR: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
