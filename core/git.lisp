;;;; PAOS - Git Worktree Management
;;;; Functions for managing Git worktrees for agent isolation

(in-package #:paos/core)

(defvar *git-command* "git"
  "The git command to use.")

(defvar *worktree-root* nil
  "Root directory for worktrees, initialized from config.")

(defun ensure-worktree-root ()
  "Ensure the worktree root directory exists and is initialized."
  (unless *worktree-root*
    (setf *worktree-root* (worktree-root)))
  (ensure-directories-exist *worktree-root*))

(defun format-worktree-path (tag)
  "Format a worktree path based on the tag name.
Returns a pathname relative to the worktree root."
  (let ((clean-tag (string-downcase (substitute #\- #\_ tag))))
    (make-pathname :directory (append (pathname-directory *worktree-root*)
                                      (list (format nil "agent-~A" clean-tag))))))

(defun worktree-exists-p (tag)
  "Check if a worktree for the given tag already exists."
  (probe-file (format-worktree-path tag)))

(defun create-worktree (tag &key (branch tag) (create-branch-p t))
  "Create a new Git worktree for the specified tag.
Returns the worktree path on success, NIL on failure."
  (ensure-worktree-root)
  (let ((worktree-path (format-worktree-path tag)))
    (when (worktree-exists-p tag)
      (log-warn "Worktree for tag '~A' already exists at ~A" tag worktree-path)
      (return-from create-worktree worktree-path))

    (handler-case
        (let* ((args (build-worktree-args worktree-path branch create-branch-p))
               (process (uiop:launch-program args
                                           :output :stream
                                           :error-output :stream)))
          (unwind-protect
              (let ((exit-code (uiop:wait-process process)))
                (if (= exit-code 0)
                    (progn
                      (log-info "Successfully created worktree for tag '~A' at ~A" tag worktree-path)
                      worktree-path)
                    (let ((error-msg (read-stream-to-string
                                     (uiop:process-info-error-output process))))
                      (log-error "Failed to create worktree for tag '~A': ~A" tag error-msg)
                      nil)))
            (uiop:close-streams process)))
      (error (e)
        (log-error "Exception while creating worktree for tag '~A': ~A" tag e)
        nil))))

(defun build-worktree-args (worktree-path branch create-branch-p)
  "Build command line arguments for git worktree add."
  (let ((args (list *git-command* "worktree" "add")))
    (when create-branch-p
      (setf args (append args (list "-b" branch))))
    (if create-branch-p
        ;; When creating a new branch, use master as the base
        (append args (list (namestring worktree-path) "master"))
        ;; When not creating a branch, use the specified branch
        (append args (list (namestring worktree-path) branch)))))

(defun remove-worktree (tag)
  "Remove a Git worktree for the specified tag.
Returns T on success, NIL on failure."
  (let ((worktree-path (format-worktree-path tag)))
    (unless (worktree-exists-p tag)
      (log-warn "Worktree for tag '~A' does not exist" tag)
      (return-from remove-worktree t))

    (handler-case
        (let* ((args (list *git-command* "worktree" "remove" (namestring worktree-path)))
               (process (uiop:launch-program args
                                           :output :stream
                                           :error-output :stream)))
          (unwind-protect
              (let ((exit-code (uiop:wait-process process)))
                (if (= exit-code 0)
                    (progn
                      (log-info "Successfully removed worktree for tag '~A'" tag)
                      t)
                    (let ((error-msg (read-stream-to-string
                                     (uiop:process-info-error-output process))))
                      (log-error "Failed to remove worktree for tag '~A': ~A" tag error-msg)
                      nil)))
            (uiop:close-streams process)))
      (error (e)
        (log-error "Exception while removing worktree for tag '~A': ~A" tag e)
        nil))))

(defun list-worktrees ()
  "List all existing worktrees.
Returns a list of worktree paths."
  (handler-case
      (let* ((args (list *git-command* "worktree" "list" "--porcelain"))
             (process (uiop:launch-program args
                                         :output :stream
                                         :error-output :stream)))
        (unwind-protect
            (let ((exit-code (uiop:wait-process process)))
              (if (= exit-code 0)
                  (parse-worktree-list (uiop:process-info-output process))
                  (let ((error-msg (read-stream-to-string
                                   (uiop:process-info-error-output process))))
                    (log-error "Failed to list worktrees: ~A" error-msg)
                    nil)))
          (uiop:close-streams process)))
    (error (e)
      (log-error "Exception while listing worktrees: ~A" e)
      nil)))

(defun parse-worktree-list (stream)
  "Parse the output of git worktree list --porcelain.
Returns a list of worktree pathnames."
  (let ((worktrees '()))
    (loop for line = (read-line stream nil nil)
          while line
          do (when (starts-with-p "worktree " line)
               (let ((path (subseq line 9))) ; Remove "worktree " prefix
                 (push (pathname path) worktrees))))
    worktrees))

(defun switch-to-worktree (tag)
  "Switch to the worktree for the specified tag.
Returns T on success, NIL on failure."
  (let ((worktree-path (format-worktree-path tag)))
    (unless (worktree-exists-p tag)
      (log-error "Cannot switch to worktree for tag '~A': worktree does not exist" tag)
      (return-from switch-to-worktree nil))

    (handler-case
        (let* ((args (list *git-command* "-C" (namestring worktree-path) "status"))
               (process (uiop:launch-program args
                                           :output :stream
                                           :error-output :stream)))
          (unwind-protect
              (let ((exit-code (uiop:wait-process process)))
                (if (= exit-code 0)
                    (progn
                      (log-info "Successfully switched to worktree for tag '~A'" tag)
                      t)
                    (let ((error-msg (read-stream-to-string
                                     (uiop:process-info-error-output process))))
                      (log-error "Failed to switch to worktree for tag '~A': ~A" tag error-msg)
                      nil)))
            (uiop:close-streams process)))
      (error (e)
        (log-error "Exception while switching to worktree for tag '~A': ~A" tag e)
        nil))))

(defun get-worktree-branch (tag)
  "Get the current branch of the worktree for the specified tag."
  (let ((worktree-path (format-worktree-path tag)))
    (unless (worktree-exists-p tag)
      (return-from get-worktree-branch nil))

    (handler-case
        (let* ((args (list *git-command* "-C" (namestring worktree-path) "branch" "--show-current"))
               (process (uiop:launch-program args
                                           :output :stream
                                           :error-output :stream)))
          (unwind-protect
              (let ((exit-code (uiop:wait-process process)))
                (if (= exit-code 0)
                    (string-trim '(#\Newline #\Return #\Space)
                                 (read-stream-to-string (uiop:process-info-output process)))
                    nil))
            (uiop:close-streams process)))
      (error (e)
        (log-error "Exception while getting branch for worktree '~A': ~A" tag e)
        nil))))

;; Initialization function
(defun initialize-git-worktrees ()
  "Initialize the Git worktree system."
  (ensure-worktree-root)
  (log-info "Git worktree system initialized with root: ~A" *worktree-root*))

;; Utility functions for logging
(defun log-info (format-string &rest args)
  "Log an info message."
  (format t "[INFO] ~A~%" (apply #'format nil format-string args)))

;; Test functions for development
(defun test-worktree-creation (tag)
  "Test function to create and verify a worktree."
  (let ((path (create-worktree tag)))
    (if path
        (progn
          (format t "Worktree created successfully: ~A~%" path)
          (format t "Current branch: ~A~%" (get-worktree-branch tag))
          t)
        (progn
          (format t "Failed to create worktree for tag: ~A~%" tag)
          nil))))

(defun cleanup-test-worktrees ()
  "Clean up test worktrees."
  (dolist (worktree (list-worktrees))
    (let ((path-string (namestring worktree)))
      (when (search "agent-" path-string)
        (let ((tag (extract-tag-from-path worktree)))
          (when tag
            (remove-worktree tag)))))))

(defun extract-tag-from-path (worktree-path)
  "Extract tag from worktree path (e.g., 'agent-ui' -> 'ui')."
  (let ((path-string (namestring worktree-path)))
    (when (search "agent-" path-string)
      (let ((start-pos (+ (search "agent-" path-string) 6)))
        (subseq path-string start-pos (position #\/ path-string :start start-pos))))))
