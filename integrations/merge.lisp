;;;; PAOS - Merge Coordination
;;;; Dependency-aware PR merging orchestration

(in-package #:paos/merge)

;;; ============================================================================
;;; Dependency Graph Analysis
;;; ============================================================================

(defun analyze-pr-dependencies (prs)
  "Build dependency graph from PR metadata and worktree tasks.
Returns dependency graph as hash table: pr-number -> list of dependencies."
  (let ((dep-graph (make-hash-table :test 'equal)))
    
    (dolist (pr prs)
      (let* ((pr-num (getf pr :number))
             (tasks (getf pr :tasks))
             (deps (extract-dependencies-from-tasks tasks)))
        (setf (gethash pr-num dep-graph) deps)))
    
    dep-graph))

(defun extract-dependencies-from-tasks (tasks)
  "Extract dependency task IDs from task list."
  (let ((all-deps '()))
    (dolist (task tasks)
      (let ((deps (gethash "dependencies" task)))
        (when deps
          (setf all-deps (append all-deps deps)))))
    (remove-duplicates all-deps :test #'equal)))

(defun build-dependency-graph (prs)
  "Build complete dependency graph including cross-PR dependencies."
  (let ((graph (make-hash-table :test 'equal))
        (pr-task-map (make-hash-table :test 'equal)))
    
    ;; First pass: map tasks to PRs
    (dolist (pr prs)
      (dolist (task (getf pr :tasks))
        (setf (gethash (gethash "id" task) pr-task-map)
              (getf pr :number))))
    
    ;; Second pass: build PR-to-PR dependencies
    (dolist (pr prs)
      (let ((pr-num (getf pr :number))
            (pr-deps '()))
        
        (dolist (task (getf pr :tasks))
          (let ((task-deps (gethash "dependencies" task)))
            (when task-deps
              (dolist (dep-id task-deps)
                (let ((dep-pr (gethash dep-id pr-task-map)))
                  (when (and dep-pr (not (= dep-pr pr-num)))
                    (pushnew dep-pr pr-deps)))))))
        
        (setf (gethash pr-num graph) pr-deps)))
    
    graph))

(defun detect-circular-dependencies (dep-graph)
  "Detect circular dependencies in PR graph."
  (let ((visited (make-hash-table :test 'equal))
        (stack (make-hash-table :test 'equal))
        (cycles '()))
    
    (labels ((visit (pr-num path)
               (when (gethash pr-num stack)
                 (push (cons pr-num path) cycles)
                 (return-from visit))
               (when (gethash pr-num visited)
                 (return-from visit))
               
               (setf (gethash pr-num stack) t)
               (setf (gethash pr-num visited) t)
               
               (dolist (dep (gethash pr-num dep-graph))
                 (visit dep (cons pr-num path)))
               
               (remhash pr-num stack)))
      
      (maphash (lambda (pr-num deps)
                 (declare (ignore deps))
                 (visit pr-num '()))
               dep-graph))
    
    cycles))

;;; ============================================================================
;;; Merge Order Determination
;;; ============================================================================

(defun determine-merge-order (prs)
  "Determine safe merge order using topological sort.
Returns ordered list of PR numbers."
  (let* ((dep-graph (build-dependency-graph prs))
         (cycles (detect-circular-dependencies dep-graph)))
    
    ;; Check for circular dependencies
    (when cycles
      (log-error "Circular dependencies detected: ~A" cycles)
      (error "Cannot determine merge order due to circular dependencies"))
    
    ;; Topological sort
    (topological-sort dep-graph)))

(defun topological-sort (dep-graph)
  "Perform topological sort on dependency graph.
Returns list of PR numbers in dependency order."
  (let ((in-degree (make-hash-table :test 'equal))
        (sorted '())
        (queue '()))
    
    ;; Calculate in-degrees
    (maphash (lambda (pr deps)
               (unless (gethash pr in-degree)
                 (setf (gethash pr in-degree) 0))
               (dolist (dep deps)
                 (setf (gethash dep in-degree)
                       (1+ (or (gethash dep in-degree) 0)))))
             dep-graph)
    
    ;; Find nodes with in-degree 0
    (maphash (lambda (pr degree)
               (when (zerop degree)
                 (push pr queue)))
             in-degree)
    
    ;; Process queue
    (loop while queue
          do (let ((pr (pop queue)))
               (push pr sorted)
               
               ;; Reduce in-degree for dependents
               (dolist (dep (gethash pr dep-graph))
                 (decf (gethash dep in-degree))
                 (when (zerop (gethash dep in-degree))
                   (push dep queue)))))
    
    (nreverse sorted)))

(defun sort-prs-by-deps (prs)
  "Sort PRs by dependencies (wrapper for backward compatibility)."
  (let ((order (determine-merge-order prs)))
    (sort (copy-list prs) 
          (lambda (a b)
            (< (position (getf a :number) order)
               (position (getf b :number) order))))))

;;; ============================================================================
;;; Merge Execution
;;; ============================================================================

(defun execute-merges (prs &key (dry-run nil) base-branch)
  "Execute merges in dependency order.
If dry-run is T, only simulate merges."
  (let ((order (determine-merge-order prs))
        (results '()))
    
    (format t "~%~AMerge sequence (~A PRs):~A~%"
            paos/dashboard::*ansi-cyan*
            (length order)
            paos/dashboard::*ansi-reset*)
    
    (loop for pr-num in order
          for i from 1
          do (format t "  ~A. PR #~A~%" i pr-num))
    
    (format t "~%")
    
    (unless dry-run
      (dolist (pr-num order)
        (let ((result (merge-single-pr pr-num :base base-branch)))
          (push (cons pr-num result) results))))
    
    (nreverse results)))

(defun merge-single-pr (pr-number &key base)
  "Merge a single PR and handle conflicts."
  (handler-case
      (progn
        (log-info "Merging PR #~A..." pr-number)
        
        (let ((args (list "gh" "pr" "merge" (format nil "~A" pr-number)
                         "--merge")))  ; Use merge commit
          (when base
            (setf args (append args (list "--base" base))))
          
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program args
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
            
            (if (zerop exit-code)
                (progn
                  (log-info "Successfully merged PR #~A" pr-number)
                  (list :status "success"
                        :pr pr-number
                        :message "Merged successfully"))
                (progn
                  (log-error "Merge failed for PR #~A: ~A" pr-number error-output)
                  (handle-merge-failure pr-number error-output))))))
    (error (e)
      (log-error "Merge error for PR #~A: ~A" pr-number e)
      (list :status "error"
            :pr pr-number
            :error (format nil "~A" e)))))

;;; ============================================================================
;;; Conflict Handling
;;; ============================================================================

(defun handle-merge-failure (pr-number error-output)
  "Handle merge failure, including conflicts."
  (cond
    ;; Merge conflict
    ((or (search "CONFLICT" error-output)
         (search "conflict" error-output))
     (let ((conflicts (detect-merge-conflicts pr-number)))
       (list :status "conflict"
             :pr pr-number
             :conflicts conflicts
             :message "Merge conflict detected"
             :action "Manual resolution required")))
    
    ;; Other failure
    (t
     (list :status "failed"
           :pr pr-number
           :error error-output
           :action "Review error and retry"))))

(defun detect-merge-conflicts (pr-number)
  "Detect which files have merge conflicts."
  (handler-case
      (let ((output (uiop:run-program
                    (list "git" "diff" "--name-only" "--diff-filter=U")
                    :output :string)))
        (remove-if #'str:empty? (str:lines output)))
    (error ()
      '("Unable to detect conflict files"))))

(defun resolve-conflict-interactive (pr-number conflicted-files)
  "Start interactive conflict resolution."
  (format t "~%~A⚠️  MERGE CONFLICT - PR #~A~A~%~%"
          paos/dashboard::*ansi-red*
          pr-number
          paos/dashboard::*ansi-reset*)
  
  (format t "Conflicted files:~%")
  (dolist (file conflicted-files)
    (format t "  - ~A~%" file))
  
  (format t "~%Options:~%")
  (format t "  1. Resolve manually~%")
  (format t "  2. Abort merge~%")
  (format t "  3. Skip and continue~%~%")
  
  ;; In real implementation, would handle user input
  (list :action "manual-resolution-required"
        :pr pr-number
        :files conflicted-files))

;;; ============================================================================
;;; Merge Coordination
;;; ============================================================================

(defun merge-all-prs (worktrees &key dry-run base-branch auto-resolve)
  "Coordinate merging all PRs from worktrees.
Complete workflow: detect conflicts, determine order, execute merges."
  (let ((prs (worktrees-to-pr-list worktrees)))
    
    ;; Step 1: Detect conflicts
    (format t "~%Step 1: Detecting conflicts...~%")
    (let ((conflicts (paos/conflicts:detect-conflicts worktrees)))
      (when conflicts
        (paos/conflicts:display-conflicts conflicts)
        (unless auto-resolve
          (error "Conflicts detected - resolve before merging"))))
    
    ;; Step 2: Determine order
    (format t "~%Step 2: Determining merge order...~%")
    (let ((order (determine-merge-order prs)))
      (format t "Order: ~{PR #~A~^ → ~}~%~%" order)
      
      ;; Step 3: Execute merges
      (if dry-run
          (format t "DRY RUN - Would merge ~A PRs~%~%" (length order))
          (progn
            (format t "~%Step 3: Executing merges...~%~%")
            (execute-merges prs :base-branch base-branch))))))

(defun worktrees-to-pr-list (worktrees)
  "Convert worktrees to PR list for merging."
  (mapcar (lambda (wt)
            (list :number (or (getf wt :pr-number)
                             (random 10000))  ; Mock for testing
                  :tag (getf wt :tag)
                  :tasks (getf wt :tasks)))
          worktrees))

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
