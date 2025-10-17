;;;; PAOS - Agent Spawning Test Suite
;;;; End-to-end testing of Zellij integration and Claude Code spawning

(in-package #:cl-user)

(defpackage #:paos/tests/spawning
  (:use #:cl #:paos/zellij)
  (:export #:run-all-tests
           #:test-zellij-detection
           #:test-session-creation
           #:test-tab-creation
           #:test-context-file-generation
           #:test-agent-spawning
           #:test-error-handling
           #:test-retry-logic))

(in-package #:paos/tests/spawning)

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(defvar *test-results* '()
  "List of test results.")

(defvar *test-session-name* "paos-test-session"
  "Name of test Zellij session.")

(defvar *test-worktree-path* "/tmp/paos-test-worktree"
  "Path to test worktree.")

(defun reset-test-results ()
  "Clear test results."
  (setf *test-results* '()))

(defun record-test (name passed &optional message)
  "Record a test result."
  (push (list :name name
              :passed passed
              :message message
              :timestamp (get-universal-time))
        *test-results*))

(defun report-test-results ()
  "Print test results summary."
  (let ((total (length *test-results*))
        (passed (count-if (lambda (r) (getf r :passed)) *test-results*)))
    (format t "~%~A========================================~A~%"
            paos/dashboard::*ansi-bold*
            paos/dashboard::*ansi-reset*)
    (format t "~ATEST RESULTS~A~%"
            paos/dashboard::*ansi-bold*
            paos/dashboard::*ansi-reset*)
    (format t "~A========================================~A~%"
            paos/dashboard::*ansi-bold*
            paos/dashboard::*ansi-reset*)

    (dolist (result (reverse *test-results*))
      (let ((status (if (getf result :passed)
                        (format nil "~A✓ PASS~A"
                               paos/dashboard::*ansi-green*
                               paos/dashboard::*ansi-reset*)
                        (format nil "~A✗ FAIL~A"
                               paos/dashboard::*ansi-red*
                               paos/dashboard::*ansi-reset*))))
        (format t "~A ~A~%" status (getf result :name))
        (when (getf result :message)
          (format t "  → ~A~%" (getf result :message)))))

    (format t "~%~A----------------------------------------~A~%"
            paos/dashboard::*ansi-bold*
            paos/dashboard::*ansi-reset*)
    (format t "Total: ~A/~A passed (~A%)~%"
            passed total
            (if (zerop total) 0 (round (* 100 (/ passed total)))))
    (format t "~A========================================~A~%~%"
            paos/dashboard::*ansi-bold*
            paos/dashboard::*ansi-reset*)

    (= passed total)))

;;; ============================================================================
;;; Individual Tests
;;; ============================================================================

(defun test-zellij-detection ()
  "Test 1: Verify Zellij is installed and accessible."
  (format t "~%Running Test 1: Zellij Detection...~%")
  (let ((detected (paos/zellij:check-zellij-installed)))
    (record-test "Zellij Detection"
                 detected
                 (if detected
                     "Zellij found and accessible"
                     "Zellij not found - install from https://zellij.dev"))
    detected))

(defun test-session-creation ()
  "Test 2: Create and verify Zellij session."
  (format t "~%Running Test 2: Session Creation...~%")
  (let ((session (paos/zellij:create-zellij-session
                  :session-name *test-session-name*)))
    (if session
        (let ((exists (paos/zellij:session-exists-p *test-session-name*)))
          (record-test "Session Creation"
                       exists
                       (if exists
                           (format nil "Session '~A' created successfully"
                                  *test-session-name*)
                           "Session created but not found in session list"))
          exists)
        (progn
          (record-test "Session Creation" nil "Failed to create session")
          nil))))

(defun test-tab-creation ()
  "Test 3: Create tabs in Zellij session."
  (format t "~%Running Test 3: Tab Creation...~%")
  (let* ((test-tags '("frontend" "backend" "database"))
         (created-tabs '()))

    (dolist (tag test-tags)
      (let ((tab-name (paos/zellij:create-zellij-tab
                       tag
                       :session-name *test-session-name*)))
        (when tab-name
          (push tab-name created-tabs))))

    (let ((success (= (length created-tabs) (length test-tags))))
      (record-test "Tab Creation"
                   success
                   (format nil "Created ~A/~A tabs: ~{~A~^, ~}"
                          (length created-tabs)
                          (length test-tags)
                          (reverse created-tabs)))
      success)))

(defun test-context-file-generation ()
  "Test 4: Generate agent context files."
  (format t "~%Running Test 4: Context File Generation...~%")

  ;; Ensure test worktree directory exists
  (ensure-directories-exist *test-worktree-path*)

  (let* ((mock-tasks (list
                      (let ((ht (make-hash-table :test 'equal)))
                        (setf (gethash "title" ht) "Implement login API")
                        (setf (gethash "description" ht) "Create JWT-based authentication")
                        ht)
                      (let ((ht (make-hash-table :test 'equal)))
                        (setf (gethash "title" ht) "Add user registration")
                        (setf (gethash "description" ht) "Support email/password signup")
                        ht)))
         (context-file (paos/zellij:create-agent-context
                        "test-agent"
                        mock-tasks
                        *test-worktree-path*)))

    (if (and context-file (probe-file context-file))
        (progn
          (record-test "Context File Generation"
                       t
                       (format nil "Context file created at ~A" context-file))
          t)
        (progn
          (record-test "Context File Generation"
                       nil
                       "Failed to create context file")
          nil))))

(defun test-agent-spawning ()
  "Test 5: Spawn agent with full workflow."
  (format t "~%Running Test 5: Agent Spawning...~%")

  (let* ((tag "test-spawn")
         (context-file (format nil "~A/.paos/context-~A.md"
                              *test-worktree-path* tag))
         (agent-info (paos/zellij:spawn-agent
                      tag
                      *test-worktree-path*
                      context-file
                      :session-name *test-session-name*)))

    (if (paos/zellij:validate-spawn-result agent-info)
        (progn
          (record-test "Agent Spawning"
                       t
                       (format nil "Agent '~A' spawned in tab '~A'"
                              tag (getf agent-info :tab-name)))
          t)
        (progn
          (record-test "Agent Spawning"
                       nil
                       "Spawn result validation failed")
          nil))))

(defun test-error-handling ()
  "Test 6: Verify error handling for invalid inputs."
  (format t "~%Running Test 6: Error Handling...~%")

  (let ((errors-caught 0)
        (expected-errors 3))

    ;; Test 1: Invalid worktree path
    (let ((result (paos/zellij:spawn-agent
                   "error-test-1"
                   "/nonexistent/path/to/nowhere"
                   nil
                   :session-name *test-session-name*)))
      (unless result (incf errors-caught)))

    ;; Test 2: Invalid session name
    (let ((result (paos/zellij:create-zellij-tab
                   "error-test-2"
                   :session-name "nonexistent-session-xyz")))
      (unless result (incf errors-caught)))

    ;; Test 3: Invalid context file path
    (let ((result (paos/zellij:create-agent-context
                   "error-test-3"
                   '()
                   "/invalid/path")))
      (unless result (incf errors-caught)))

    (let ((success (= errors-caught expected-errors)))
      (record-test "Error Handling"
                   success
                   (format nil "Caught ~A/~A expected errors"
                          errors-caught expected-errors))
      success)))

(defun test-retry-logic ()
  "Test 7: Verify retry mechanism works."
  (format t "~%Running Test 7: Retry Logic...~%")

  ;; This test would normally fail but should retry
  (let ((result (paos/zellij:spawn-agent-with-retry
                 "retry-test"
                 *test-worktree-path*
                 (format nil "~A/.paos/context-retry-test.md"
                        *test-worktree-path*)
                 :max-retries 2
                 :session-name *test-session-name*)))

    ;; If we got a result, retry logic is working
    (record-test "Retry Logic"
                 (not (null result))
                 (if result
                     "Retry mechanism succeeded"
                     "Retry mechanism exhausted attempts"))
    (not (null result))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-all-tests ()
  "Run all agent spawning tests."
  (reset-test-results)

  (format t "~%~A========================================~A~%"
          paos/dashboard::*ansi-bold*
          paos/dashboard::*ansi-reset*)
  (format t "~APAOS AGENT SPAWNING TEST SUITE~A~%"
          paos/dashboard::*ansi-bold*
          paos/dashboard::*ansi-reset*)
  (format t "~A========================================~A~%"
          paos/dashboard::*ansi-bold*
          paos/dashboard::*ansi-reset*)

  ;; Run tests in sequence
  (test-zellij-detection)
  (test-session-creation)
  (test-tab-creation)
  (test-context-file-generation)
  (test-agent-spawning)
  (test-error-handling)
  (test-retry-logic)

  ;; Report results
  (let ((all-passed (report-test-results)))

    ;; Cleanup
    (format t "~%Cleaning up test environment...~%")
    (cleanup-test-environment)

    all-passed))

(defun cleanup-test-environment ()
  "Clean up test session and files."
  ;; Note: Zellij sessions should be closed manually
  ;; We don't auto-close to avoid disrupting user's workflow
  (format t "~%~ANOTE:~A Test session '~A' left running for inspection.~%"
          paos/dashboard::*ansi-yellow*
          paos/dashboard::*ansi-reset*
          *test-session-name*)
  (format t "To close: ~Azellij delete-session ~A~A~%"
          paos/dashboard::*ansi-cyan*
          *test-session-name*
          paos/dashboard::*ansi-reset*)

  ;; Clean up test worktree
  (when (probe-file *test-worktree-path*)
    (uiop:delete-directory-tree (pathname *test-worktree-path*)
                                :validate t
                                :if-does-not-exist :ignore)
    (format t "Cleaned up test worktree: ~A~%" *test-worktree-path*)))

;;; ============================================================================
;;; Quick Test Runner
;;; ============================================================================

(defun quick-test ()
  "Run a quick smoke test."
  (format t "~%Running quick smoke test...~%")
  (and (test-zellij-detection)
       (test-session-creation)
       (test-tab-creation)))
