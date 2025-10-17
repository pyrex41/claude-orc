# PAOS Test Suite

## Agent Spawning Tests

Comprehensive end-to-end tests for the Zellij integration and Claude Code agent spawning functionality.

### Prerequisites

1. **Zellij must be installed:**
   ```bash
   # macOS
   brew install zellij

   # Or download from https://zellij.dev
   ```

2. **PAOS system loaded:**
   ```bash
   sbcl --load paos.asd
   ```

### Running Tests

#### Option 1: Run Full Test Suite (Recommended)

```lisp
;; Load the test suite
(load "tests/test-agent-spawning.lisp")

;; Run all tests
(paos/tests/spawning:run-all-tests)
```

#### Option 2: Quick Smoke Test

```lisp
(load "tests/test-agent-spawning.lisp")
(paos/tests/spawning:quick-test)
```

#### Option 3: Individual Tests

```lisp
(load "tests/test-agent-spawning.lisp")

;; Run specific tests
(paos/tests/spawning:test-zellij-detection)
(paos/tests/spawning:test-session-creation)
(paos/tests/spawning:test-tab-creation)
(paos/tests/spawning:test-context-file-generation)
(paos/tests/spawning:test-agent-spawning)
(paos/tests/spawning:test-error-handling)
(paos/tests/spawning:test-retry-logic)
```

### Test Coverage

The test suite validates:

1. **✓ Zellij Detection** - Verifies Zellij is installed and accessible
2. **✓ Session Creation** - Creates and verifies Zellij test session
3. **✓ Tab Creation** - Creates multiple tabs with formatted names
4. **✓ Context File Generation** - Generates markdown context files for agents
5. **✓ Agent Spawning** - Full workflow: tab → cd → claude --context
6. **✓ Error Handling** - Validates graceful failure handling
7. **✓ Retry Logic** - Tests retry mechanism for failed spawns

### Expected Output

```
========================================
PAOS AGENT SPAWNING TEST SUITE
========================================

Running Test 1: Zellij Detection...
INFO: Zellij detected: zellij 0.39.2

Running Test 2: Session Creation...
INFO: Created Zellij session: paos-test-session

Running Test 3: Tab Creation...
INFO: Created Zellij tab: frontend
INFO: Created Zellij tab: backend
INFO: Created Zellij tab: database

Running Test 4: Context File Generation...
INFO: Created agent context: /tmp/paos-test-worktree/.paos/context-test-agent.md

Running Test 5: Agent Spawning...
INFO: Created Zellij tab: test-spawn
INFO: Agent 'test-spawn' spawned successfully

Running Test 6: Error Handling...
ERROR: Failed to spawn agent for error-test-1: ...
ERROR: Failed to create Zellij tab for error-test-2: ...
ERROR: Failed to create context file for error-test-3: ...

Running Test 7: Retry Logic...
INFO: Retry attempt 1 for retry-test...

========================================
TEST RESULTS
========================================
✓ PASS Zellij Detection
  → Zellij found and accessible
✓ PASS Session Creation
  → Session 'paos-test-session' created successfully
✓ PASS Tab Creation
  → Created 3/3 tabs: frontend, backend, database
✓ PASS Context File Generation
  → Context file created at /tmp/paos-test-worktree/.paos/context-test-agent.md
✓ PASS Agent Spawning
  → Agent 'test-spawn' spawned in tab 'test-spawn'
✓ PASS Error Handling
  → Caught 3/3 expected errors
✓ PASS Retry Logic
  → Retry mechanism succeeded

----------------------------------------
Total: 7/7 passed (100%)
========================================
```

### Cleanup

The test suite creates:
- A Zellij session named `paos-test-session`
- Test tabs within that session
- Test files in `/tmp/paos-test-worktree/`

**Test files are automatically cleaned up**, but the Zellij session is left running for inspection.

To manually clean up:
```bash
# Close the test session
zellij delete-session paos-test-session

# Or attach to inspect it first
zellij attach paos-test-session
```

### Troubleshooting

#### "Zellij not found"
Install Zellij: https://zellij.dev/documentation/installation

#### "Session already exists"
Delete the existing test session:
```bash
zellij delete-session paos-test-session
```

#### Tests fail with "command not found: claude"
The tests verify the spawning mechanism, not Claude Code availability. If Claude Code isn't installed, the spawn command will be sent but the agent won't start. This is expected behavior for testing the integration itself.

### Development

To add new tests:

1. Add test function to `test-agent-spawning.lisp`
2. Follow naming convention: `test-<feature-name>`
3. Use `record-test` to log results
4. Add to `run-all-tests` function

Example:
```lisp
(defun test-new-feature ()
  "Test X: Description of test."
  (format t "~%Running Test X: Feature Name...~%")
  (let ((result (test-implementation)))
    (record-test "Feature Name"
                 result
                 (if result "Success message" "Failure message"))
    result))
```
