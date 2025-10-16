# PAOS API Documentation

## Core API

### Configuration
```lisp
(paos/core:load-config)  ; Load .paos/config.yaml
(paos/core:api-key "anthropic")  ; Get API key
```

### PRD Processing
```lisp
;; Parse PRD (auto-detect format)
(paos/prd-parser:parse-prd "path/to/prd.md")

;; AI-enhanced parsing
(paos/prd-parser:parse-prd-with-ai "path/to/prd.txt")

;; Format-specific
(paos/prd-parser:parse-markdown-prd "file.md")
(paos/prd-parser:parse-yaml-prd "file.yaml")
(paos/prd-parser:parse-plain-text-prd "file.txt")
```

### Task Organization
```lisp
;; Tag tasks for parallel execution
(paos/tagger:tag-and-group tasks :use-ai t)

;; Expand into subtasks
(paos/expander:expand-task task :max-depth 3)
(paos/expander:expand-all-tasks tasks)
```

### Git Worktrees
```lisp
(paos/core:create-worktree "ui-team")
(paos/core:list-worktrees)
(paos/core:remove-worktree "ui-team")
```

### Agent Management
```lisp
;; Spawn agent in Zellij tab
(paos/zellij:spawn-agent tag worktree-path context-file)

;; Control agents
(paos/zellij:send-command-to-agent agent "pause")
(paos/zellij:focus-agent "ui-team")
```

### Status Monitoring
```lisp
;; Write status (from agent)
(paos/status:update-agent-status tag
  :status "running"
  :progress 50
  :current-task "Building UI")

;; Poll status (from orchestrator)
(paos/status:poll-agent-statuses '("ui-team" "backend-team"))
(paos/status:poll-all-statuses)

;; Start monitoring
(paos/status:start-status-monitor tags callback-fn)
```

### Intelligence
```lisp
;; Analyze agent statuses
(paos/ai:analyze-agent-statuses statuses)

;; Start continuous analysis
(paos/ai:start-continuous-analysis status-fn :interval 60)
```

### Conflict Detection
```lisp
;; Detect conflicts
(paos/conflicts:detect-conflicts worktrees)

;; Monitor continuously
(paos/conflicts:start-conflict-monitoring worktrees-fn)
```

### Pull Requests
```lisp
;; Create PR
(paos/github:create-pr tag worktree :use-ai t)

;; Batch creation
(paos/github:create-prs-for-worktrees worktrees)

;; Review PRs
(paos/review:review-prs '(123 124 125))

;; Merge with coordination
(paos/merge:merge-all-prs worktrees)
```

### Main Interface
```lisp
;; Start full orchestrator
(paos:main)

;; Or custom startup
(paos:start-interface :mode :split)
```

## Configuration File

```yaml
# .paos/config.yaml
api_keys:
  anthropic: ""  # Use ANTHROPIC_API_KEY env var
  openai: ""

worktree:
  root: ".paos/worktrees"

agents:
  max_concurrent: 5
  timeout: 3600

checkpoints:
  pre_commit: true
  at_blockers: true
  after_subtask: false
  on_error: true
```

## Environment Variables

- `ANTHROPIC_API_KEY` - Required for AI features
- `PAOS_CONFIG_PATH` - Override config location
- `PAOS_LOG_LEVEL` - Logging verbosity

## Status File Format

```json
{
  "tag": "ui-team",
  "status": "running",
  "progress": 50,
  "current_task": "Building components",
  "completed_tasks": ["Setup", "Config"],
  "errors": [],
  "last_update": 1234567890,
  "metadata": {}
}
```

## Tool Definitions (ACI)

```lisp
(paos/aci:define-tool "tool_name"
  "Description of what tool does"
  parameters-hash  ; JSON schema
  handler-function
  :category "git"
  :safe t)
```

## Common Workflows

### Complete Development Cycle
```lisp
;; 1. Parse and plan
(let ((tasks (paos/prd-parser:parse-prd-with-ai "prd.md")))
  (let ((organized (paos/tagger:tag-and-group tasks :use-ai t)))
    
    ;; 2. Expand and prepare
    (paos/expander:expand-all-tasks tasks)
    
    ;; 3. Spawn agents
    (dolist (group (getf organized :groups))
      (let ((tag (getf group :tag)))
        (paos/core:create-worktree tag)
        (paos/zellij:spawn-agent tag ...)))
    
    ;; 4. Monitor and control
    (paos:start-interface :mode :split)))
```

### Error Handling Pattern
```lisp
(handler-case
    (risky-operation)
  (error (e)
    (paos/usability:enhanced-error :api-error
      (format nil "Operation failed: ~A" e)
      "Check API key and try again")))
```
