;;;; PAOS - Review Assistance
;;;; AI-powered PR review and merge order recommendations

(in-package #:paos/review)

;;; ============================================================================
;;; PR Diff Fetching
;;; ============================================================================

(defun fetch-pr-diffs (pr-numbers)
  "Fetch diffs for multiple PRs.
Returns list of (pr-number . diff) pairs."
  (let ((results '()))
    (dolist (pr-num pr-numbers)
      (let ((diff (fetch-single-pr-diff pr-num)))
        (when diff
          (push (cons pr-num diff) results))))
    (nreverse results)))

(defun fetch-single-pr-diff (pr-number)
  "Fetch diff for a single PR using GitHub CLI."
  (handler-case
      (let ((output (uiop:run-program
                    (list "gh" "pr" "diff" (format nil "~A" pr-number))
                    :output :string)))
        output)
    (error (e)
      (log-error "Failed to fetch diff for PR #~A: ~A" pr-number e)
      nil)))

(defun fetch-pr-metadata (pr-number)
  "Fetch PR metadata (title, author, labels, etc.)."
  (handler-case
      (let ((output (uiop:run-program
                    (list "gh" "pr" "view" (format nil "~A" pr-number)
                         "--json" "number,title,author,labels,state,url,body")
                    :output :string)))
        (cl-json:decode-json-from-string output))
    (error (e)
      (log-error "Failed to fetch metadata for PR #~A: ~A" pr-number e)
      nil)))

(defun fetch-all-open-prs ()
  "Fetch all open PRs in current repository.
Returns list of PR numbers."
  (handler-case
      (let ((output (uiop:run-program
                    '("gh" "pr" "list" "--json" "number" "--limit" "100")
                    :output :string)))
        (let ((prs (cl-json:decode-json-from-string output)))
          (mapcar (lambda (pr) (cdr (assoc :number pr))) prs)))
    (error (e)
      (log-error "Failed to fetch PR list: ~A" e)
      '())))

;;; ============================================================================
;;; Claude API Analysis
;;; ============================================================================

(defun review-prs (pr-numbers)
  "Review multiple PRs using Claude API.
Returns analysis with insights and merge order suggestions."
  (handler-case
      (let* ((pr-diffs (fetch-pr-diffs pr-numbers))
             (pr-metadata (mapcar #'fetch-pr-metadata pr-numbers))
             (api-key (paos/core:get-api-key))
             (prompt (build-review-prompt pr-diffs pr-metadata))
             (response (paos/core:call-claude-api api-key prompt :max-tokens 8192))
             (analysis (parse-review-response response)))
        analysis)
    (error (e)
      (log-error "PR review failed: ~A" e)
      (create-fallback-review pr-numbers))))

(defun build-review-prompt (pr-diffs pr-metadata)
  "Build comprehensive review prompt for Claude."
  (with-output-to-string (stream)
    (format stream "You are an expert code reviewer analyzing multiple pull requests.~%~%")
    
    ;; PR Overview
    (format stream "## Pull Requests to Review~%~%")
    (loop for metadata in pr-metadata
          for (pr-num . diff) in pr-diffs
          do (format stream "### PR #~A: ~A~%"
                    pr-num
                    (cdr (assoc :title metadata)))
             (format stream "Author: ~A~%"
                    (cdr (assoc :login (cdr (assoc :author metadata)))))
             (format stream "State: ~A~%"
                    (cdr (assoc :state metadata)))
             (format stream "~%**Diff:**~%```diff~%~A~%```~%~%"
                    (or diff "(no diff available)")))
    
    ;; Analysis Instructions
    (format stream "~%## Review Task~%~%")
    (format stream "Analyze these PRs and provide:~%~%")
    (format stream "Return JSON:~%")
    (format stream "{~%")
    (format stream "  \"overall_assessment\": {~%")
    (format stream "    \"quality\": \"excellent|good|fair|poor\",~%")
    (format stream "    \"summary\": \"Brief overall assessment\"~%")
    (format stream "  },~%")
    (format stream "  \"pr_reviews\": [~%")
    (format stream "    {~%")
    (format stream "      \"pr_number\": 123,~%")
    (format stream "      \"title\": \"PR title\",~%")
    (format stream "      \"issues\": [{\"severity\": \"high|medium|low\", \"issue\": \"...\", \"location\": \"file:line\"}],~%")
    (format stream "      \"strengths\": [\"good aspects\"],~%")
    (format stream "      \"suggestions\": [\"improvements\"],~%")
    (format stream "      \"approve_status\": \"approve|request_changes|comment\"~%")
    (format stream "    }~%")
    (format stream "  ],~%")
    (format stream "  \"merge_order\": [~%")
    (format stream "    {\"pr_number\": 123, \"reason\": \"why this order\"}~%")
    (format stream "  ],~%")
    (format stream "  \"conflicts\": [~%")
    (format stream "    {\"prs\": [123, 456], \"type\": \"merge|logical\", \"description\": \"...\"}~%")
    (format stream "  ]~%")
    (format stream "}~%")))

;;; Note: call-claude-api now provided by paos/core

(defun parse-review-response (response)
  "Parse Claude's review analysis."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response)))
             (text (cdr (assoc :text (car content)))))
        (let ((json-start (position #\{ text))
              (json-end (position #\} text :from-end t)))
          (when (and json-start json-end)
            (cl-json:decode-json-from-string
             (subseq text json-start (1+ json-end))))))
    (error (e)
      (log-error "Failed to parse review response: ~A" e)
      nil)))

;;; ============================================================================
;;; Review Insights
;;; ============================================================================

(defun generate-review-insights (analysis)
  "Generate actionable insights from review analysis."
  (let ((overall (cdr (assoc :overall-assessment analysis)))
        (reviews (cdr (assoc :pr-reviews analysis)))
        (merge-order (cdr (assoc :merge-order analysis)))
        (conflicts (cdr (assoc :conflicts analysis))))
    
    (list :quality (cdr (assoc :quality overall))
          :summary (cdr (assoc :summary overall))
          :critical-issues (extract-critical-issues reviews)
          :blocking-prs (identify-blocking-prs reviews)
          :safe-to-merge (identify-safe-prs reviews)
          :merge-sequence (format-merge-order merge-order)
          :conflicts conflicts)))

(defun extract-critical-issues (reviews)
  "Extract high severity issues from reviews."
  (let ((critical '()))
    (dolist (review reviews)
      (let ((pr-num (cdr (assoc :pr-number review)))
            (issues (cdr (assoc :issues review))))
        (dolist (issue issues)
          (when (string= (cdr (assoc :severity issue)) "high")
            (push (list :pr pr-num
                       :issue (cdr (assoc :issue issue))
                       :location (cdr (assoc :location issue)))
                  critical)))))
    (nreverse critical)))

(defun identify-blocking-prs (reviews)
  "Identify PRs that should block merging."
  (remove-if-not
   (lambda (review)
     (string= (cdr (assoc :approve-status review)) "request_changes"))
   reviews))

(defun identify-safe-prs (reviews)
  "Identify PRs safe to merge."
  (remove-if-not
   (lambda (review)
     (string= (cdr (assoc :approve-status review)) "approve"))
   reviews))

(defun format-merge-order (merge-order)
  "Format merge order with reasoning."
  (mapcar (lambda (item)
            (list :pr (cdr (assoc :pr-number item))
                  :reason (cdr (assoc :reason item))))
          merge-order))

;;; ============================================================================
;;; Fallback Review
;;; ============================================================================

(defun create-fallback-review (pr-numbers)
  "Create basic review when AI unavailable."
  (let ((reviews '()))
    (dolist (pr-num pr-numbers)
      (let ((metadata (fetch-pr-metadata pr-num)))
        (when metadata
          (push (list :pr-number pr-num
                     :title (cdr (assoc :title metadata))
                     :issues '()
                     :strengths '("Automated review unavailable")
                     :suggestions '("Manual review recommended")
                     :approve-status "comment")
                reviews))))
    
    (list (cons :overall-assessment
                (list :quality "unknown"
                     :summary "AI review unavailable - manual review needed"))
          (cons :pr-reviews (nreverse reviews))
          (cons :merge-order (mapcar (lambda (n) (list :pr-number n
                                                      :reason "Default order"))
                                    pr-numbers))
          (cons :conflicts '()))))

;;; ============================================================================
;;; Display Functions
;;; ============================================================================

(defun display-review-insights (insights)
  "Display review insights in formatted way."
  (format t "~%~A=== PR REVIEW INSIGHTS ===~A~%~%"
          paos/dashboard::*ansi-cyan*
          paos/dashboard::*ansi-reset*)
  
  ;; Overall quality
  (let ((quality (getf insights :quality)))
    (format t "~AOverall Quality: ~A~A~%"
            (quality-to-color quality)
            (string-upcase quality)
            paos/dashboard::*ansi-reset*)
    (format t "~A~%~%" (getf insights :summary)))
  
  ;; Critical issues
  (let ((critical (getf insights :critical-issues)))
    (when critical
      (format t "~A⚠️  CRITICAL ISSUES~A~%"
              paos/dashboard::*ansi-red*
              paos/dashboard::*ansi-reset*)
      (dolist (issue critical)
        (format t "  PR #~A: ~A~%"
                (getf issue :pr)
                (getf issue :issue))
        (when (getf issue :location)
          (format t "    Location: ~A~%" (getf issue :location))))
      (format t "~%")))
  
  ;; Merge sequence
  (let ((sequence (getf insights :merge-sequence)))
    (when sequence
      (format t "~A📋 RECOMMENDED MERGE ORDER~A~%"
              paos/dashboard::*ansi-green*
              paos/dashboard::*ansi-reset*)
      (loop for item in sequence
            for i from 1
            do (format t "  ~A. PR #~A - ~A~%"
                      i
                      (getf item :pr)
                      (getf item :reason)))
      (format t "~%")))
  
  ;; Conflicts
  (let ((conflicts (getf insights :conflicts)))
    (when conflicts
      (format t "~A⚡ CONFLICTS DETECTED~A~%"
              paos/dashboard::*ansi-yellow*
              paos/dashboard::*ansi-reset*)
      (dolist (conflict conflicts)
        (format t "  PRs ~{#~A~^, ~}: ~A~%"
                (cdr (assoc :prs conflict))
                (cdr (assoc :description conflict))))
      (format t "~%"))))

(defun quality-to-color (quality)
  "Convert quality to color."
  (cond
    ((string= quality "excellent") paos/dashboard::*ansi-green*)
    ((string= quality "good") paos/dashboard::*ansi-cyan*)
    ((string= quality "fair") paos/dashboard::*ansi-yellow*)
    ((string= quality "poor") paos/dashboard::*ansi-red*)
    (t paos/dashboard::*ansi-white*)))

;;; ============================================================================
;;; Export and Reporting
;;; ============================================================================

(defun export-review-report (analysis output-file)
  "Export review analysis to file."
  (with-open-file (stream output-file
                         :direction :output
                         :if-exists :supersede)
    (write-string (cl-json:encode-json-to-string analysis) stream)))

(defun summarize-review (analysis)
  "Create executive summary of review."
  (let ((overall (cdr (assoc :overall-assessment analysis)))
        (reviews (cdr (assoc :pr-reviews analysis))))
    (format nil "Quality: ~A | Total PRs: ~A | Approved: ~A | Changes Requested: ~A"
            (string-upcase (cdr (assoc :quality overall)))
            (length reviews)
            (count-if (lambda (r)
                       (string= (cdr (assoc :approve-status r)) "approve"))
                     reviews)
            (count-if (lambda (r)
                       (string= (cdr (assoc :approve-status r)) "request_changes"))
                     reviews))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================
;;; Note: get-api-key and call-claude-api now provided by paos/core

(defun log-error (format-string &rest args)
  "Log error message."
  (format *error-output* "ERROR: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
