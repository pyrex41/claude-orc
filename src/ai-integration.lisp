;;;; PAOS - AI Integration
;;;; Claude API integration for PRD analysis and enhancement

(in-package #:paos/prd-parser)

;;; ============================================================================
;;; Claude API Integration for PRD Analysis
;;; ============================================================================

(defvar *claude-api-base* "https://api.anthropic.com/v1/messages"
  "Base URL for Claude API.")

(defvar *claude-model* "claude-3-5-sonnet-20241022"
  "Default Claude model for PRD analysis.")

(defun analyze-prd-with-claude (prd-content)
  "Use Claude API to analyze and enhance PRD content.
Returns enhanced task structure with AI-generated insights."
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (prompt (build-prd-analysis-prompt prd-content))
             (response (paos/core:call-claude-api api-key prompt)))
        (parse-claude-response response))
    (error (e)
      (log-error "Claude API analysis failed: ~A" e)
      nil)))

;;; Note: get-api-key now provided by paos/core

(defun build-prd-analysis-prompt (prd-content)
  "Build a prompt for Claude to analyze PRD and extract structured tasks."
  (format nil "You are a software engineering expert analyzing a Product Requirements Document.

PRD Content:
~A

Please analyze this PRD and extract structured tasks in JSON format. For each task, provide:
- title: Clear, concise task title
- description: Detailed description of what needs to be done
- priority: high, medium, or low
- dependencies: List of task IDs this depends on (if identifiable)
- estimatedComplexity: low, medium, high, or very-high
- technicalNotes: Any technical considerations or implementation notes

Also extract:
- metadata: Overview, goals, requirements summary
- riskFactors: Potential risks or challenges
- suggestedApproach: High-level implementation approach

Return ONLY valid JSON in this format:
{
  \"tasks\": [...],
  \"metadata\": {...},
  \"riskFactors\": [...],
  \"suggestedApproach\": \"...\"
}" prd-content))

;;; Note: call-claude-api now provided by paos/core

(defun parse-claude-response (response)
  "Parse Claude API response and extract task data."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response)))
             (text (cdr (assoc :text (car content)))))
        ;; Extract JSON from the text (Claude might wrap it in markdown)
        (let ((json-start (position #\{ text))
              (json-end (position #\} text :from-end t)))
          (if (and json-start json-end)
              (let ((json-str (subseq text json-start (1+ json-end))))
                (cl-json:decode-json-from-string json-str))
              (error "No valid JSON found in Claude response"))))
    (error (e)
      (log-error "Failed to parse Claude response: ~A" e)
      nil)))

;;; ============================================================================
;;; Enhanced PRD Parsing with Claude
;;; ============================================================================

(defun parse-prd-with-ai (file-path)
  "Parse PRD with AI enhancement. First tries format-specific parser,
then enhances with Claude API analysis."
  (let* ((basic-parse (paos/prd-parser:parse-prd file-path))
         (content (read-file-to-string file-path))
         (ai-analysis (analyze-prd-with-claude content)))
    
    (if ai-analysis
        (merge-parsed-data basic-parse ai-analysis)
        basic-parse)))  ; Fallback to basic parse if AI fails

(defun merge-parsed-data (basic-data ai-data)
  "Merge basic parsed data with AI-enhanced data."
  (let ((result (or basic-data (make-hash-table :test 'equal))))
    
    ;; Merge tasks - prefer AI-enhanced tasks if available
    (let ((ai-tasks (cdr (assoc :tasks ai-data)))
          (basic-tasks (gethash "tasks" basic-data)))
      (setf (gethash "tasks" result)
            (or (convert-ai-tasks-to-hash ai-tasks)
                basic-tasks
                '())))
    
    ;; Add AI-specific enhancements
    (when (assoc :*risk-factors ai-data)
      (setf (gethash "riskFactors" result)
            (cdr (assoc :*risk-factors ai-data))))
    
    (when (assoc :*suggested-approach ai-data)
      (setf (gethash "suggestedApproach" result)
            (cdr (assoc :*suggested-approach ai-data))))
    
    ;; Merge metadata
    (let ((ai-metadata (cdr (assoc :metadata ai-data)))
          (basic-metadata (gethash "metadata" basic-data)))
      (when (or ai-metadata basic-metadata)
        (setf (gethash "metadata" result)
              (merge-metadata basic-metadata ai-metadata))))
    
    result))

(defun convert-ai-tasks-to-hash (ai-tasks)
  "Convert AI-generated task alist to hash table format."
  (when ai-tasks
    (mapcar (lambda (task-alist)
              (let ((task (make-hash-table :test 'equal)))
                (dolist (pair task-alist)
                  (setf (gethash (string-downcase (symbol-name (car pair))) task)
                        (cdr pair)))
                task))
            ai-tasks)))

(defun merge-metadata (basic-meta ai-meta)
  "Merge basic and AI metadata, preferring more detailed AI data."
  (let ((result (or basic-meta (make-hash-table :test 'equal))))
    (when ai-meta
      (dolist (pair ai-meta)
        (let ((key (string-downcase (symbol-name (car pair))))
              (value (cdr pair)))
          (unless (gethash key result)
            (setf (gethash key result) value)))))
    result))

;;; ============================================================================
;;; PRD Quality Analysis
;;; ============================================================================

(defun analyze-prd-quality (file-path)
  "Analyze PRD quality and completeness using Claude."
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (content (read-file-to-string file-path))
             (prompt (build-quality-analysis-prompt content))
             (response (paos/core:call-claude-api api-key prompt)))
        (parse-quality-response response))
    (error (e)
      (log-error "PRD quality analysis failed: ~A" e)
      nil)))

(defun build-quality-analysis-prompt (prd-content)
  "Build prompt for PRD quality analysis."
  (format nil "You are a product requirements expert. Analyze this PRD for quality and completeness.

PRD Content:
~A

Provide analysis in JSON format:
{
  \"completeness\": {\"score\": 0-100, \"missing\": [\"list of missing elements\"]},
  \"clarity\": {\"score\": 0-100, \"issues\": [\"clarity issues\"]},
  \"feasibility\": {\"score\": 0-100, \"concerns\": [\"technical concerns\"]},
  \"recommendations\": [\"list of improvement suggestions\"]
}

Return ONLY valid JSON." prd-content))

(defun parse-quality-response (response)
  "Parse Claude's PRD quality analysis response."
  (parse-claude-response response))  ; Uses same parsing logic

;;; ============================================================================
;;; Ambiguity Resolution
;;; ============================================================================

(defun resolve-ambiguous-requirements (requirements-text)
  "Use Claude to clarify ambiguous requirements."
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (prompt (format nil "Clarify and make specific these ambiguous requirements:

~A

Provide clear, actionable, testable requirements." requirements-text))
             (response (paos/core:call-claude-api api-key prompt)))
        (extract-text-from-response response))
    (error (e)
      (log-error "Requirement clarification failed: ~A" e)
      requirements-text)))  ; Return original on failure

(defun extract-text-from-response (response)
  "Extract text content from Claude response."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response))))
        (cdr (assoc :text (car content))))
    (error (e)
      (log-error "Failed to extract text from response: ~A" e)
      nil)))
