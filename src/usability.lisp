;;;; PAOS - Usability Improvements
;;;; Enhanced help system and error messages

(in-package #:paos/usability)

;;; ============================================================================
;;; Comprehensive Help System
;;; ============================================================================

(defvar *help-topics* (make-hash-table :test 'equal)
  "Registry of help topics.")

(defclass help-topic ()
  ((name :initarg :name :accessor topic-name)
   (description :initarg :description :accessor topic-description)
   (content :initarg :content :accessor topic-content)
   (examples :initarg :examples :accessor topic-examples :initform '())
   (see-also :initarg :see-also :accessor topic-see-also :initform '())))

(defun register-help-topic (name description content &key examples see-also)
  "Register a help topic."
  (setf (gethash name *help-topics*)
        (make-instance 'help-topic
                      :name name
                      :description description
                      :content content
                      :examples examples
                      :see-also see-also)))

;; Register core topics
(register-help-topic 
 "commands"
 "Available REPL commands"
 "PAOS provides an interactive REPL with the following command categories:

**Status Commands:** status, agents, tasks
**Agent Control:** pause, resume, switch, chat
**Dashboard:** refresh, dashboard
**Feedback:** feedback, review
**Core:** help, quit, exit"
 :examples '("help commands" "status" "agents")
 :see-also '("agents" "dashboard"))

(register-help-topic
 "workflow"
 "Complete PAOS workflow"
 "1. Parse PRD: (decompose-prd \"prd.txt\")
2. Tag tasks: (tag-and-group tasks :use-ai t)
3. Expand tasks: (expand-all-tasks tasks)
4. Create worktrees: (create-worktree tag)
5. Spawn agents: (spawn-agent tag path context)
6. Monitor: Dashboard + REPL
7. Review: (review-prs pr-numbers)
8. Merge: (merge-all-prs worktrees)"
 :see-also '("commands" "agents"))

(register-help-topic
 "troubleshooting"
 "Common issues and solutions"
 "**Agent not responding:**
- Check Zellij session: zellij list-sessions
- Switch to agent tab: switch <agent>
- Check status file: cat .paos/status/<agent>.json

**API errors:**
- Verify ANTHROPIC_API_KEY is set
- Check API key encryption status
- Review audit log: .paos/audit.log

**Conflicts:**
- List conflicts: conflicts show
- Resolve manually in worktrees
- Use conflict detection before merge"
 :see-also '("commands" "security"))

(defun help-system (&optional topic)
  "Display help information."
  (if topic
      (show-help-topic topic)
      (show-help-index)))

(defun show-help-index ()
  "Show index of all help topics."
  (format t "~%~A=== PAOS HELP SYSTEM ===~A~%~%"
          paos/dashboard::*ansi-cyan*
          paos/dashboard::*ansi-reset*)
  
  (format t "Available topics:~%~%")
  (maphash (lambda (name topic)
             (format t "  ~A~A~A - ~A~%"
                    paos/dashboard::*ansi-green*
                    name
                    paos/dashboard::*ansi-reset*
                    (topic-description topic)))
           *help-topics*)
  
  (format t "~%Usage: help <topic>~%"))

(defun show-help-topic (topic-name)
  "Show detailed help for specific topic."
  (let ((topic (gethash topic-name *help-topics*)))
    (if topic
        (progn
          (format t "~%~A=== ~A ===~A~%~%"
                  paos/dashboard::*ansi-cyan*
                  (string-upcase (topic-name topic))
                  paos/dashboard::*ansi-reset*)
          (format t "~A~%~%" (topic-content topic))
          
          (when (topic-examples topic)
            (format t "~AExamples:~A~%"
                    paos/dashboard::*ansi-yellow*
                    paos/dashboard::*ansi-reset*)
            (dolist (ex (topic-examples topic))
              (format t "  $ ~A~%" ex))
            (format t "~%"))
          
          (when (topic-see-also topic)
            (format t "~ASee also:~A ~{~A~^, ~}~%~%"
                    paos/dashboard::*ansi-cyan*
                    paos/dashboard::*ansi-reset*
                    (topic-see-also topic))))
        (format t "~AUnknown topic: ~A~A~%Try 'help' for topic list.~%~%"
                paos/dashboard::*ansi-yellow*
                topic-name
                paos/dashboard::*ansi-reset*))))

;;; ============================================================================
;;; Enhanced Error Messages
;;; ============================================================================

(defun enhanced-error (error-type context &optional suggestion)
  "Format enhanced error message with context and suggestions."
  (format t "~%~A❌ ERROR: ~A~A~%"
          paos/dashboard::*ansi-red*
          (error-type-description error-type)
          paos/dashboard::*ansi-reset*)
  (format t "~AContext:~A ~A~%"
          paos/dashboard::*ansi-yellow*
          paos/dashboard::*ansi-reset*
          context)
  (when suggestion
    (format t "~A💡 Suggestion:~A ~A~%"
            paos/dashboard::*ansi-green*
            paos/dashboard::*ansi-reset*
            suggestion))
  (format t "~%"))

(defun error-type-description (error-type)
  "Get description for error type."
  (case error-type
    (:api-error "API Call Failed")
    (:config-error "Configuration Error")
    (:git-error "Git Operation Failed")
    (:agent-error "Agent Error")
    (:parse-error "Parsing Error")
    (t "Unknown Error")))

(defun suggest-command (partial-command)
  "Suggest commands based on partial input."
  (let* ((all-commands '("status" "agents" "tasks" "pause" "resume"
                        "switch" "chat" "refresh" "dashboard" "feedback"
                        "review" "help" "quit" "exit"))
         (matches (remove-if-not
                   (lambda (cmd)
                     (str:starts-with? partial-command cmd))
                   all-commands)))
    (when matches
      (format t "~ADid you mean:~A ~{~A~^, ~}?~%"
              paos/dashboard::*ansi-yellow*
              paos/dashboard::*ansi-reset*
              matches))))

;;; ============================================================================
;;; Command Discovery
;;; ============================================================================

(defun command-discovery ()
  "Interactive command discovery."
  (format t "~%~A=== COMMAND DISCOVERY ===~A~%~%"
          paos/dashboard::*ansi-cyan*
          paos/dashboard::*ansi-reset*)
  
  (format t "What would you like to do?~%~%")
  (format t "  1. Check agent status~%")
  (format t "  2. Control agents (pause/resume)~%")
  (format t "  3. View dashboard~%")
  (format t "  4. Review PRs~%")
  (format t "  5. Get help~%~%")
  
  (format t "Enter number or command name: "))

(defun auto-complete (partial)
  "Auto-complete command names."
  (let* ((commands '("status" "agents" "tasks" "pause" "resume" "switch"
                    "chat" "refresh" "dashboard" "feedback" "review"
                    "help" "quit" "exit"))
         (matches (remove-if-not
                   (lambda (cmd)
                     (str:starts-with? partial cmd))
                   commands)))
    (cond
      ((null matches) nil)
      ((= (length matches) 1) (car matches))
      (t matches))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
