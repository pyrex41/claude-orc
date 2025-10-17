;;;; PAOS - Main Orchestrator Interface
;;;; Combined dashboard and REPL interface

(in-package #:paos)

;;; ============================================================================
;;; Main Interface
;;; ============================================================================

(defvar *interface-running* nil
  "Flag indicating if main interface is active.")

(defvar *interface-mode* :split
  "Interface mode: :split (dashboard+repl), :dashboard-only, :repl-only.")

(defclass orchestrator-interface ()
  ((dashboard :accessor interface-dashboard)
   (repl-context :accessor interface-repl)
   (agents :accessor interface-agents :initform '())
   (worktrees :accessor interface-worktrees :initform '())
   (mode :initarg :mode :accessor interface-mode :initform :split)))

(defun create-interface (&key (mode :split))
  "Create orchestrator interface instance."
  (make-instance 'orchestrator-interface :mode mode))

;;; ============================================================================
;;; Interface Startup
;;; ============================================================================

(defun start-interface (&key (mode :split) agent-status-fn)
  "Start the main PAOS interface.
mode can be :split, :dashboard-only, or :repl-only."
  (when *interface-running*
    (log-warn "Interface already running")
    (return-from start-interface nil))
  
  (let ((interface (create-interface :mode mode)))
    (setf *interface-running* t)
    
    ;; Initialize security
    (paos/security:setup-sandbox)
    (paos/security:initialize-encryption)
    
    ;; Try to recover previous session
    (let ((recovered (paos/reliability:recover-session)))
      (when recovered
        (format t "~ARecovered previous session~A~%~%"
                paos/dashboard::*ansi-green*
                paos/dashboard::*ansi-reset*)
        (setf (interface-agents interface) 
              (paos/reliability:state-agents recovered))))
    
    ;; Start background services
    (paos/reliability:start-auto-save :interval 60)
    
    (case mode
      (:split (run-split-interface interface agent-status-fn))
      (:dashboard-only (run-dashboard-only interface agent-status-fn))
      (:repl-only (run-repl-only interface)))))

(defun stop-interface ()
  "Stop the main interface and cleanup."
  (when *interface-running*
    (setf *interface-running* nil)
    (paos/reliability:safe-shutdown)
    (log-info "Interface stopped")))

;;; ============================================================================
;;; Split Mode (Dashboard + REPL)
;;; ============================================================================

(defun run-split-interface (interface agent-status-fn)
  "Run interface with both dashboard and REPL.
Dashboard updates in background, REPL in foreground."
  (unwind-protect
       (progn
         ;; Start dashboard in background
         (let ((dashboard (paos/dashboard:start-dashboard 
                          (or agent-status-fn
                              (lambda () (interface-agents interface)))
                          :refresh-interval 2)))
           (setf (interface-dashboard interface) dashboard))
         
         ;; Start status monitor
         (paos/status:start-status-monitor 
          '() ;; agent tags
          (lambda (statuses)
            (setf (interface-agents interface) statuses))
          :interval 2)
         
         ;; Start analysis
         (paos/ai:start-continuous-analysis
          (lambda () (interface-agents interface))
          :interval 60
          :callback #'display-analysis-callback)
         
         ;; Start conflict monitoring
         (paos/conflicts:start-conflict-monitoring
          (lambda () (interface-worktrees interface))
          :interval 30
          :callback #'display-conflict-callback)
         
         ;; Run REPL in foreground (blocks)
         (let ((repl-ctx (make-instance 'paos/human-interface::repl-context
                                       :dashboard (interface-dashboard interface))))
           (setf (interface-repl interface) repl-ctx)
           (paos/human-interface::repl-loop repl-ctx)))
    
    ;; Cleanup
    (cleanup-interface interface)))

(defun display-analysis-callback (analysis)
  "Callback for analysis results."
  (let ((health (getf analysis :health)))
    (when (and health (< (getf health :score) 50))
      (format t "~%~A⚠️  Low health score: ~A/100~A~%"
              paos/dashboard::*ansi-yellow*
              (getf health :score)
              paos/dashboard::*ansi-reset*))))

(defun display-conflict-callback (conflicts)
  "Callback for conflict detection."
  (when conflicts
    (format t "~%~A⚠️  ~A conflicts detected~A~%"
            paos/dashboard::*ansi-red*
            (length conflicts)
            paos/dashboard::*ansi-reset*)))

;;; ============================================================================
;;; Dashboard-Only Mode
;;; ============================================================================

(defun run-dashboard-only (interface agent-status-fn)
  "Run dashboard without REPL."
  (unwind-protect
       (progn
         (let ((dashboard (paos/dashboard:start-dashboard agent-status-fn)))
           (setf (interface-dashboard interface) dashboard))
         
         ;; Wait for interrupt
         (format t "~%Dashboard running. Press Ctrl+C to stop.~%~%")
         (loop while *interface-running*
               do (sleep 1)))
    
    (cleanup-interface interface)))

;;; ============================================================================
;;; REPL-Only Mode
;;; ============================================================================

(defun run-repl-only (interface)
  "Run REPL without dashboard."
  (unwind-protect
       (let ((repl-ctx (make-instance 'paos/human-interface::repl-context)))
         (setf (interface-repl interface) repl-ctx)
         (paos/human-interface::start-repl))
    
    (cleanup-interface interface)))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun cleanup-interface (interface)
  "Cleanup interface resources."
  (declare (ignore interface))
  
  ;; Stop all background services
  (paos/dashboard:stop-dashboard)
  (paos/status:stop-status-monitor)
  (paos/ai:stop-continuous-analysis)
  (paos/conflicts:stop-conflict-monitoring)
  (paos/reliability:stop-auto-save)
  
  (log-info "Interface cleanup complete"))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(defun main ()
  "Main entry point for PAOS orchestrator."
  (handler-case
      (progn
        (print-banner)
        
        ;; Load configuration
        (paos/core:load-config)
        
        ;; Check prerequisites
        (check-prerequisites)
        
        ;; Start interface
        (start-interface :mode :split))
    (error (e)
      (format t "~%~AFATAL ERROR: ~A~A~%"
              paos/dashboard::*ansi-red*
              e
              paos/dashboard::*ansi-reset*)
      (safe-shutdown-on-error))))

(defun print-banner ()
  "Print PAOS startup banner."
  (format t "~%~A" paos/dashboard::*ansi-cyan*)
  (format t "╔═══════════════════════════════════════════════════╗~%")
  (format t "║                                                   ║~%")
  (format t "║   PAOS - Parallel Agent Orchestration System     ║~%")
  (format t "║   v0.1.0                                          ║~%")
  (format t "║                                                   ║~%")
  (format t "╚═══════════════════════════════════════════════════╝~%")
  (format t "~A~%" paos/dashboard::*ansi-reset*))

(defun check-prerequisites ()
  "Check that all prerequisites are available."
  (format t "~%Checking prerequisites...~%")
  
  ;; Check Zellij
  (unless (paos/zellij:check-zellij-installed)
    (log-warn "Zellij not installed - agent spawning unavailable"))
  
  ;; Check GitHub CLI
  (unless (paos/github:check-gh-cli)
    (log-warn "GitHub CLI not installed - PR features unavailable"))
  
  ;; Check API key
  (unless (uiop:getenv "ANTHROPIC_API_KEY")
    (log-warn "ANTHROPIC_API_KEY not set - AI features limited"))
  
  (format t "~%"))

(defun safe-shutdown-on-error ()
  "Safely shutdown on error."
  (when *interface-running*
    (setf *interface-running* nil)
    (paos/reliability:safe-shutdown)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun log-warn (format-string &rest args)
  "Log warning message."
  (format *error-output* "WARN: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
