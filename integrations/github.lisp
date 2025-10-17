;;;; PAOS - GitHub Integration
;;;; Automated PR creation and management via GitHub CLI

(in-package #:paos/github)

;;; ============================================================================
;;; Pull Request Creation
;;; ============================================================================

(defun create-pr (tag worktree &key title body labels reviewers base)
  "Create a pull request for a worktree using GitHub CLI.
Returns PR URL or NIL on failure."
  (handler-case
      (let* ((pr-title (or title (generate-pr-title tag worktree)))
             (pr-body (or body (generate-pr-body tag worktree)))
             (pr-args (build-pr-args pr-title pr-body
                                   :labels labels
                                   :reviewers reviewers
                                   :base base))
             (path (getf worktree :path)))
        
        (log-info "Creating PR for ~A..." tag)
        
        (let ((result (uiop:run-program pr-args
                                      :output :string
                                      :directory path
                                      :error-output :string
                                      :ignore-error-status t)))
          (multiple-value-bind (output error-output exit-code)
              (values result nil 0)
            (if (zerop exit-code)
                (progn
                  (log-info "PR created successfully for ~A" tag)
                  (extract-pr-url output))
                (progn
                  (log-error "PR creation failed for ~A: ~A" tag error-output)
                  nil)))))
    (error (e)
      (log-error "PR creation error for ~A: ~A" tag e)
      nil)))

(defun build-pr-args (title body &key labels reviewers base)
  "Build GitHub CLI arguments for PR creation."
  (let ((args (list "gh" "pr" "create"
                   "--title" title
                   "--body" body)))
    
    ;; Add base branch if specified
    (when base
      (setf args (append args (list "--base" base))))
    
    ;; Add labels
    (when labels
      (dolist (label labels)
        (setf args (append args (list "--label" label)))))
    
    ;; Add reviewers
    (when reviewers
      (dolist (reviewer reviewers)
        (setf args (append args (list "--reviewer" reviewer)))))
    
    args))

(defun extract-pr-url (gh-output)
  "Extract PR URL from gh output."
  (let ((lines (str:lines (str:trim gh-output))))
    (car (last lines))))  ; URL is usually on last line

;;; ============================================================================
;;; PR Content Generation
;;; ============================================================================

(defun generate-pr-title (tag worktree)
  "Generate PR title from worktree context."
  (let* ((tasks (getf worktree :tasks))
         (task-count (length tasks))
         (first-task (car tasks)))
    (if first-task
        (format nil "~A: ~A (~A task~:P)"
                (str:capitalize tag)
                (gethash "title" first-task)
                task-count)
        (format nil "~A: Implementation" (str:capitalize tag)))))

(defun generate-pr-body (tag worktree)
  "Generate detailed PR body with task context."
  (with-output-to-string (stream)
    (let ((tasks (getf worktree :tasks))
          (branch (getf worktree :branch)))
      
      ;; Summary section
      (format stream "## Summary~%~%")
      (format stream "Implementation for **~A** agent group.~%~%" tag)
      
      ;; Tasks section
      (format stream "## Tasks Completed~%~%")
      (dolist (task tasks)
        (format stream "- [x] ~A~%" (gethash "title" task))
        (when (gethash "subtasks" task)
          (dolist (subtask (gethash "subtasks" task))
            (format stream "  - [x] ~A~%" (gethash "title" subtask)))))
      (format stream "~%")
      
      ;; Changes section
      (format stream "## Changes~%~%")
      (let ((files (get-changed-files worktree)))
        (if files
            (progn
              (format stream "Modified files:~%")
              (dolist (file files)
                (format stream "- `~A`~%" file))
              (format stream "~%"))
            (format stream "*(No files changed)*~%~%")))
      
      ;; Test plan
      (format stream "## Test Plan~%~%")
      (format stream "- [ ] Unit tests pass~%")
      (format stream "- [ ] Integration tests pass~%")
      (format stream "- [ ] Manual testing completed~%~%")
      
      ;; Additional context
      (format stream "## Additional Context~%~%")
      (format stream "- **Branch**: `~A`~%" (or branch "unknown"))
      (format stream "- **Agent Tag**: `~A`~%" tag)
      (format stream "- **Worktree**: `~A`~%~%" (getf worktree :path))
      
      ;; Dependencies
      (when (has-dependencies-p tasks)
        (format stream "## Dependencies~%~%")
        (format stream "This PR depends on:~%")
        (dolist (dep (extract-pr-dependencies tasks))
          (format stream "- ~A~%" dep))
        (format stream "~%"))
      
      ;; Auto-generated footer
      (format stream "---~%")
      (format stream "*Auto-generated by PAOS Orchestrator*~%"))))

(defun get-changed-files (worktree)
  "Get list of changed files in worktree."
  (let ((path (getf worktree :path)))
    (handler-case
        (let ((output (uiop:run-program
                      (list "git" "-C" path "diff" "--name-only" "HEAD")
                      :output :string)))
          (remove-if #'str:empty? (str:lines output)))
      (error () '()))))

(defun has-dependencies-p (tasks)
  "Check if tasks have dependencies."
  (some (lambda (task) (gethash "dependencies" task)) tasks))

(defun extract-pr-dependencies (tasks)
  "Extract dependency information from tasks."
  (let ((deps '()))
    (dolist (task tasks)
      (let ((task-deps (gethash "dependencies" task)))
        (when task-deps
          (dolist (dep task-deps)
            (pushnew (format nil "Task ~A" dep) deps :test #'string=)))))
    deps))

;;; ============================================================================
;;; Rich Context Formatting
;;; ============================================================================

(defun format-pr-with-ai (tag worktree)
  "Use Claude to generate rich PR description."
  (handler-case
      (let* ((api-key (paos/core:get-api-key))
             (prompt (build-pr-generation-prompt tag worktree))
             (response (paos/core:call-claude-api api-key prompt))
             (pr-content (parse-pr-response response)))
        pr-content)
    (error (e)
      (log-warn "AI PR generation failed, using template: ~A" e)
      (list :title (generate-pr-title tag worktree)
            :body (generate-pr-body tag worktree)))))

(defun build-pr-generation-prompt (tag worktree)
  "Build prompt for AI-generated PR content."
  (format nil "Generate a professional pull request for this implementation:

Agent Tag: ~A
Tasks:
~{- ~A~%~}

Files Changed:
~{- ~A~%~}

Create a clear, professional PR with:
1. Concise title (under 72 chars)
2. Summary of changes
3. List of completed tasks
4. Technical details
5. Test plan checklist

Return JSON:
{
  \"title\": \"PR title\",
  \"body\": \"PR body in markdown\",
  \"labels\": [\"suggested\", \"labels\"],
  \"highlights\": [\"key changes\"]
}"
          tag
          (mapcar (lambda (task) (gethash "title" task))
                 (getf worktree :tasks))
          (get-changed-files worktree)))

;;; Note: call-claude-api now provided by paos/core

(defun parse-pr-response (response)
  "Parse Claude's PR generation response."
  (handler-case
      (let* ((json-response (cl-json:decode-json-from-string response))
             (content (cdr (assoc :content json-response)))
             (text (cdr (assoc :text (car content)))))
        (let ((json-start (position #\{ text))
              (json-end (position #\} text :from-end t)))
          (when (and json-start json-end)
            (let ((parsed (cl-json:decode-json-from-string
                          (subseq text json-start (1+ json-end)))))
              (list :title (cdr (assoc :title parsed))
                    :body (cdr (assoc :body parsed))
                    :labels (cdr (assoc :labels parsed)))))))
    (error (e)
      (log-error "Failed to parse PR response: ~A" e)
      nil)))

;;; ============================================================================
;;; PR Management
;;; ============================================================================

(defun list-prs (&key state author label)
  "List pull requests."
  (let ((args (list "gh" "pr" "list")))
    (when state
      (setf args (append args (list "--state" state))))
    (when author
      (setf args (append args (list "--author" author))))
    (when label
      (setf args (append args (list "--label" label))))
    
    (handler-case
        (uiop:run-program args :output :string)
      (error (e)
        (log-error "Failed to list PRs: ~A" e)
        ""))))

(defun get-pr-status (pr-number)
  "Get status of a specific PR."
  (handler-case
      (let ((output (uiop:run-program
                    (list "gh" "pr" "view" (format nil "~A" pr-number) "--json" "state,title,url")
                    :output :string)))
        (cl-json:decode-json-from-string output))
    (error (e)
      (log-error "Failed to get PR status: ~A" e)
      nil)))

(defun merge-pr (pr-number &key method)
  "Merge a pull request."
  (let ((args (list "gh" "pr" "merge" (format nil "~A" pr-number))))
    (when method
      (setf args (append args (list "--merge" method))))
    
    (handler-case
        (uiop:run-program args :output :string)
      (error (e)
        (log-error "Failed to merge PR ~A: ~A" pr-number e)
        nil))))

;;; ============================================================================
;;; Batch PR Creation
;;; ============================================================================

(defun create-prs-for-worktrees (worktrees &key use-ai base-branch)
  "Create PRs for multiple worktrees.
Returns list of (tag . pr-url) pairs."
  (let ((results '()))
    (dolist (worktree worktrees)
      (let* ((tag (getf worktree :tag))
             (pr-content (if use-ai
                            (format-pr-with-ai tag worktree)
                            (list :title (generate-pr-title tag worktree)
                                  :body (generate-pr-body tag worktree))))
             (pr-url (create-pr tag worktree
                               :title (getf pr-content :title)
                               :body (getf pr-content :body)
                               :labels (getf pr-content :labels)
                               :base base-branch)))
        (when pr-url
          (push (cons tag pr-url) results))))
    (nreverse results)))

(defun check-gh-cli ()
  "Check if GitHub CLI is installed and authenticated."
  (handler-case
      (let ((version (uiop:run-program '("gh" "--version") :output :string)))
        (log-info "GitHub CLI detected: ~A" (str:trim version))
        t)
    (error ()
      (log-error "GitHub CLI not found. Install from https://cli.github.com")
      nil)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================
;;; Note: get-api-key and call-claude-api now provided by paos/core

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
