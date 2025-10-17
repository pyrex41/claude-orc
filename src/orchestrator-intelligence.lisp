;;;; PAOS - Orchestrator Intelligence
;;;; LLM-based analysis for agent statuses and recommendations

(in-package #:paos/ai)

;;; ============================================================================
;;; Status Formatting for LLM
;;; ============================================================================

(defun format-statuses-for-llm (statuses)
  "Format agent statuses into a structured prompt for Claude API.
Returns a formatted string suitable for LLM analysis."
  (with-output-to-string (stream)
    (format stream "# Agent Status Report~%~%")
    (format stream "Timestamp: ~A~%~%" (format-timestamp (get-universal-time)))
    
    ;; Summary section
    (let ((summary (paos/status:summarize-statuses statuses)))
      (format stream "## Summary~%")
      (format stream "- Total Agents: ~A~%" (gethash "total" summary))
      (format stream "- Running: ~A~%" (gethash "running" summary))
      (format stream "- Completed: ~A~%" (gethash "done" summary))
      (format stream "- Errors: ~A~%" (gethash "errors" summary))
      (format stream "- Average Progress: ~,1F%~%~%" 
              (gethash "avg_progress" summary)))
    
    ;; Individual agent details
    (format stream "## Agent Details~%~%")
    (dolist (status statuses)
      (format stream "### Agent: ~A~%" (paos/status:status-tag status))
      (format stream "- Status: ~A~%" (paos/status:status-state status))
      (format stream "- Progress: ~A%~%" (paos/status:status-progress status))
      (format stream "- Current Task: ~A~%" 
              (or (paos/status:status-current-task status) "None"))
      (format stream "- Completed Tasks: ~{~A~^, ~}~%" 
              (paos/status:status-completed-tasks status))
      (when (paos/status:status-errors status)
        (format stream "- Errors: ~{~A~^, ~}~%" 
                (paos/status:status-errors status)))
      (format stream "- Last Update: ~A~%~%" 
              (format-timestamp (paos/status:status-last-update status))))
    
    ;; Stale agents warning
    (let ((stale (paos/status:get-stale-agents statuses)))
      (when stale
        (format stream "## ⚠️ Stale Agents (No updates >5min)~%")
        (format stream "~{- ~A~%~}~%" stale)))))

(defun format-analysis-prompt (statuses)
  "Build complete analysis prompt for Claude."
  (format nil "You are an expert orchestrator analyzing parallel agent execution.

~A

Based on the status report above, provide analysis and recommendations in JSON format:

{
  \"health\": {
    \"overall\": \"healthy|concerning|critical\",
    \"score\": 0-100,
    \"summary\": \"Brief overall assessment\"
  },
  \"issues\": [
    {
      \"agent\": \"agent-tag\",
      \"severity\": \"low|medium|high|critical\",
      \"issue\": \"Description of the issue\",
      \"recommendation\": \"What to do about it\"
    }
  ],
  \"recommendations\": [
    {
      \"priority\": \"high|medium|low\",
      \"action\": \"Specific action to take\",
      \"reason\": \"Why this is recommended\",
      \"agents\": [\"affected-agents\"]
    }
  ],
  \"insights\": {
    \"blockers\": [\"List of blocking issues\"],
    \"opportunities\": [\"Optimization opportunities\"],
    \"risks\": [\"Potential risks identified\"]
  },
  \"next_steps\": [
    \"Prioritized list of recommended next actions\"
  ]
}

Focus on:
1. Identifying stuck or failing agents
2. Detecting performance bottlenecks
3. Suggesting resource reallocation
4. Predicting potential conflicts
5. Recommending intervention points

Return ONLY valid JSON."
          (format-statuses-for-llm statuses)))

;;; ============================================================================
;;; Claude API Integration
;;; ============================================================================

(defun analyze-agent-statuses (statuses)
  "Analyze agent statuses using Claude API and return insights."
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (prompt (format-analysis-prompt statuses))
             (response (paos/core:call-claude-api api-key prompt))
             (analysis (parse-analysis-response response)))
        analysis)
    (error (e)
      (log-error "Status analysis failed: ~A" e)
      (create-fallback-analysis statuses))))

;;; Note: call-claude-api now provided by paos/core

(defun parse-analysis-response (response)
  "Parse Claude's analysis response."
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
              (error "No valid JSON in Claude response"))))
    (error (e)
      (log-error "Failed to parse analysis response: ~A" e)
      nil)))

;;; ============================================================================
;;; Recommendation Processing
;;; ============================================================================

(defun process-analysis-recommendations (analysis)
  "Process and prioritize recommendations from LLM analysis."
  (let ((health (cdr (assoc :health analysis)))
        (issues (cdr (assoc :issues analysis)))
        (recommendations (cdr (assoc :recommendations analysis)))
        (insights (cdr (assoc :insights analysis)))
        (next-steps (cdr (assoc :next-steps analysis))))
    
    (list :health (process-health-status health)
          :critical-issues (filter-critical-issues issues)
          :high-priority-actions (filter-high-priority recommendations)
          :all-recommendations recommendations
          :insights insights
          :next-steps next-steps)))

(defun process-health-status (health)
  "Process health status from analysis."
  (when health
    (let ((overall (cdr (assoc :overall health)))
          (score (cdr (assoc :score health)))
          (summary (cdr (assoc :summary health))))
      (list :overall overall
            :score score
            :summary summary
            :color (health-to-color overall)))))

(defun health-to-color (health-status)
  "Convert health status to color code."
  (cond
    ((string= health-status "healthy") paos/dashboard::*ansi-green*)
    ((string= health-status "concerning") paos/dashboard::*ansi-yellow*)
    ((string= health-status "critical") paos/dashboard::*ansi-red*)
    (t paos/dashboard::*ansi-white*)))

(defun filter-critical-issues (issues)
  "Filter to only critical and high severity issues."
  (when issues
    (remove-if-not 
     (lambda (issue)
       (let ((severity (cdr (assoc :severity issue))))
         (or (string= severity "critical")
             (string= severity "high"))))
     issues)))

(defun filter-high-priority (recommendations)
  "Filter to only high priority recommendations."
  (when recommendations
    (remove-if-not
     (lambda (rec)
       (string= (cdr (assoc :priority rec)) "high"))
     recommendations)))

;;; ============================================================================
;;; Automated Actions
;;; ============================================================================

(defun execute-recommendations (processed-analysis &key auto-execute)
  "Execute or present recommendations.
If auto-execute is T, automatically execute safe recommendations."
  (let ((critical-issues (getf processed-analysis :critical-issues))
        (high-priority (getf processed-analysis :high-priority-actions)))
    
    ;; Log critical issues
    (when critical-issues
      (dolist (issue critical-issues)
        (log-warn "CRITICAL: ~A - ~A" 
                 (cdr (assoc :agent issue))
                 (cdr (assoc :issue issue)))))
    
    ;; Process high priority actions
    (when high-priority
      (dolist (action high-priority)
        (let ((action-text (cdr (assoc :action action)))
              (reason (cdr (assoc :reason action)))
              (agents (cdr (assoc :agents action))))
          
          (if auto-execute
              (execute-action action-text agents)
              (present-recommendation action-text reason agents)))))))

(defun execute-action (action agents)
  "Execute an automated action on agents."
  ;; Placeholder for automated actions
  (log-info "Would execute: ~A on agents: ~{~A~^, ~}" action agents))

(defun present-recommendation (action reason agents)
  "Present recommendation to user for approval."
  (format t "~%~ARECOMMENDATION~A~%"
          paos/dashboard::*ansi-cyan*
          paos/dashboard::*ansi-reset*)
  (format t "Action: ~A~%" action)
  (format t "Reason: ~A~%" reason)
  (format t "Affects: ~{~A~^, ~}~%~%" agents))

;;; ============================================================================
;;; Fallback Analysis
;;; ============================================================================

(defun create-fallback-analysis (statuses)
  "Create simple rule-based analysis when AI is unavailable."
  (let ((summary (paos/status:summarize-statuses statuses))
        (stale (paos/status:get-stale-agents statuses))
        (issues '())
        (recommendations '()))
    
    ;; Detect stale agents
    (when stale
      (dolist (tag stale)
        (push (list :agent tag
                   :severity "high"
                   :issue "Agent has not reported status recently"
                   :recommendation "Check agent health and restart if needed")
              issues)))
    
    ;; Check for errors
    (dolist (status statuses)
      (when (paos/status:status-errors status)
        (push (list :agent (paos/status:status-tag status)
                   :severity "high"
                   :issue (format nil "Errors: ~{~A~^, ~}" 
                                (paos/status:status-errors status))
                   :recommendation "Review error logs and debug")
              issues)))
    
    ;; Check progress
    (let ((avg-progress (gethash "avg_progress" summary)))
      (when (< avg-progress 25)
        (push (list :priority "medium"
                   :action "Monitor slow progress"
                   :reason "Average progress below 25%"
                   :agents (mapcar #'paos/status:status-tag statuses))
              recommendations)))
    
    (list (cons :health (list :overall "unknown"
                             :score 50
                             :summary "Fallback analysis (AI unavailable)"))
          (cons :issues issues)
          (cons :recommendations recommendations)
          (cons :insights (list :blockers stale
                               :opportunities '()
                               :risks '()))
          (cons :next-steps (list "Check stale agents"
                                "Review error logs"
                                "Monitor progress")))))

;;; ============================================================================
;;; Continuous Analysis Loop
;;; ============================================================================

(defvar *analysis-running* nil
  "Flag for analysis loop.")

(defvar *analysis-thread* nil
  "Analysis thread.")

(defvar *last-analysis* nil
  "Cache of last analysis result.")

(defun start-continuous-analysis (status-fn &key (interval 60) callback)
  "Start continuous analysis of agent statuses.
status-fn should return list of agent statuses.
callback is called with analysis results."
  (when *analysis-running*
    (log-warn "Analysis already running")
    (return-from start-continuous-analysis nil))
  
  (setf *analysis-running* t)
  (setf *analysis-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *analysis-running*
                 do (handler-case
                        (let* ((statuses (funcall status-fn))
                               (analysis (analyze-agent-statuses statuses))
                               (processed (process-analysis-recommendations analysis)))
                          (setf *last-analysis* processed)
                          (when callback
                            (funcall callback processed))
                          (sleep interval))
                      (error (e)
                        (log-error "Analysis loop error: ~A" e)
                        (sleep interval)))))
         :name "paos-analysis"))
  (log-info "Continuous analysis started (interval: ~As)" interval))

(defun stop-continuous-analysis ()
  "Stop the continuous analysis loop."
  (when *analysis-running*
    (setf *analysis-running* nil)
    (when *analysis-thread*
      (bordeaux-threads:join-thread *analysis-thread* :timeout 5)
      (setf *analysis-thread* nil))
    (log-info "Continuous analysis stopped")))

(defun get-latest-analysis ()
  "Get the most recent analysis result."
  *last-analysis*)

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun format-timestamp (universal-time)
  "Format timestamp for display."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

;;; Note: get-api-key now provided by paos/core

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

;;; ============================================================================
;;; Display Functions
;;; ============================================================================

(defun display-analysis (processed-analysis)
  "Display analysis results in a formatted way."
  (let ((health (getf processed-analysis :health))
        (critical (getf processed-analysis :critical-issues))
        (recommendations (getf processed-analysis :high-priority-actions))
        (next-steps (getf processed-analysis :next-steps)))
    
    ;; Health status
    (format t "~%~A=== ORCHESTRATOR ANALYSIS ===~A~%~%"
            paos/dashboard::*ansi-cyan*
            paos/dashboard::*ansi-reset*)
    
    (when health
      (format t "~AHealth: ~A (~A/100)~A~%"
              (getf health :color)
              (getf health :overall)
              (getf health :score)
              paos/dashboard::*ansi-reset*)
      (format t "~A~%~%" (getf health :summary)))
    
    ;; Critical issues
    (when critical
      (format t "~A⚠️  CRITICAL ISSUES~A~%"
              paos/dashboard::*ansi-red*
              paos/dashboard::*ansi-reset*)
      (dolist (issue critical)
        (format t "  • ~A: ~A~%"
                (cdr (assoc :agent issue))
                (cdr (assoc :issue issue))))
      (format t "~%"))
    
    ;; High priority recommendations
    (when recommendations
      (format t "~A📋 HIGH PRIORITY ACTIONS~A~%"
              paos/dashboard::*ansi-yellow*
              paos/dashboard::*ansi-reset*)
      (dolist (rec recommendations)
        (format t "  • ~A~%"
                (cdr (assoc :action rec))))
      (format t "~%"))
    
    ;; Next steps
    (when next-steps
      (format t "~A🎯 NEXT STEPS~A~%"
              paos/dashboard::*ansi-green*
              paos/dashboard::*ansi-reset*)
      (loop for step in next-steps
            for i from 1
            do (format t "  ~A. ~A~%" i step))
      (format t "~%"))))
