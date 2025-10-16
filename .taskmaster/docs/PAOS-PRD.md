# Product Requirements Document: Parallel Agent Orchestration System (PAOS)

**Version:** 1.0  
**Date:** October 16, 2025  
**Author:** Product Team  
**Status:** Draft for Review

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Problem Statement](#2-problem-statement)
3. [Product Goals & Success Metrics](#3-product-goals--success-metrics)
4. [User Personas & Use Cases](#4-user-personas--use-cases)
5. [Functional Requirements](#5-functional-requirements)
6. [Non-Functional Requirements](#6-non-functional-requirements)
7. [Technical Architecture & Language Choice](#7-technical-architecture--language-choice)
8. [User Workflows](#8-user-workflows)
9. [Edge Cases & Error Handling](#9-edge-cases--error-handling)
10. [Future Enhancements](#10-future-enhancements)
11. [Implementation Plan](#11-implementation-plan)
12. [Open Questions & Decisions](#12-open-questions--decisions)
13. [Appendices](#13-appendices)

---

## 1. Executive Summary

### Product Vision

A terminal-based orchestration system that decomposes complex software development tasks from PRDs into parallelizable work streams, spawns isolated AI agents (Claude Code instances) in separate Git worktrees, and provides real-time monitoring and coordination—all while maintaining developer control and visibility.

### Target Users

Individual developers and small teams working on complex, multi-faceted codebases who want to leverage AI agents for parallel execution while maintaining human oversight.

### Core Value Proposition

Transform weeks of sequential development into days of parallel execution by intelligently decomposing tasks and running multiple AI agents simultaneously in isolated environments.

### Key Differentiators

- **True Parallelism:** Multiple Claude Code instances working simultaneously on independent task trees
- **Git Worktree Isolation:** Complete code separation prevents agent conflicts
- **Terminal-First UX:** Zellij-based interface provides visibility without framework overhead
- **Human-in-the-Loop:** Developer can intervene at any point without breaking agent flow
- **Simple Architecture:** Direct LLM API calls in Common Lisp—no heavy frameworks

---

## 2. Problem Statement

### Current Pain Points

**Sequential Bottlenecks:** Complex PRDs require sequential implementation, limiting development velocity. A feature touching frontend, backend, database, and infrastructure typically requires weeks of serial work.

**Single-Threaded AI Limitations:** Existing AI coding assistants operate in single sessions, unable to effectively handle multi-file, multi-domain changes simultaneously. Context switching between domains reduces efficiency.

**Visibility Loss:** When multiple changes occur across a codebase, developers lose track of what's happening where. Traditional automation provides no transparency into agent reasoning or progress.

**Framework Abstraction Overhead:** Existing agent frameworks (LangChain, CrewAI, AutoGPT) add layers that obscure prompts, responses, and control flow, making debugging difficult.

**Lack of Isolation:** Without proper sandboxing, multiple AI agents risk conflicting edits, dependency collisions, and integration failures.

### Why Existing Solutions Fall Short

- **Heavy Frameworks:** Add complexity through abstraction layers that hide underlying LLM calls
- **Single-Agent Focus:** No native parallelization for independent work streams
- **Poor Visibility:** "Magic" black boxes with limited insight into decision-making
- **Missing Human Oversight:** Fully autonomous systems lack checkpoints for quality control
- **No Isolation Primitives:** Agents share workspace, causing conflicts

### The Opportunity

By combining proven patterns from Anthropic's agent research (orchestrator-workers, parallelization, evaluator-optimizer) with Git worktrees and Zellij multiplexing, we can create a system that:

- Reduces implementation time by 60-80% through intelligent parallelization
- Maintains 100% transparency via terminal-based monitoring
- Enables seamless human intervention without breaking agent state
- Prevents conflicts through filesystem-level isolation
- Uses simple, composable components instead of framework magic

---

## 3. Product Goals & Success Metrics

### Primary Goals

1. **Velocity:** Reduce time-to-implementation for complex PRDs by 60-80% through parallelization
2. **Quality:** Maintain high code quality with >85% test pass rate on first merge
3. **Transparency:** Provide 100% visibility into agent actions, reasoning, and progress
4. **Control:** Enable human intervention at any checkpoint without state loss
5. **Reliability:** Achieve >70% agent success rate without human intervention

### Success Metrics

| Metric | Target | Measurement Method |
|--------|--------|-------------------|
| **Tasks Completed Per Day** | 3-5x improvement vs. manual | Task tracking logs |
| **First-Merge Test Pass Rate** | >85% | CI/CD integration metrics |
| **Developer Daily Usage** | Daily for complex tasks | Session logs |
| **Agent Autonomy Rate** | >70% tasks without intervention | Checkpoint analysis |
| **Time to First Value** | <30 min learning curve | User onboarding surveys |
| **Conflict Rate** | <5% of parallel tasks | Git merge conflict logs |

### Non-Goals (v1)

- **Web-Based UI:** Terminal-only interface initially (defer to Phase 2)
- **Multi-Repository Orchestration:** Single repo focus (defer to Phase 3)
- **Real-Time Collaboration:** Single developer per session
- **Non-Git VCS Support:** Git-only requirement
- **Cloud/SaaS Deployment:** Local-first architecture

---

## 4. User Personas & Use Cases

### Primary Persona: "Alex, the Full-Stack Developer"

**Background:**
- 5-10 years experience
- Works on features spanning frontend, backend, database, infrastructure
- Time-constrained startup environment or large enterprise codebase
- Values transparency and control over "magic" automation
- Comfortable with terminal interfaces, Git workflows, and functional programming

**Pain Points:**
- Spends 60% of time on boilerplate across multiple domains
- Context switching between UI and API code breaks flow
- Complex features take 2-3 weeks due to sequential dependencies
- AI tools help with single files but not holistic features

**Goals:**
- Ship complete features in days, not weeks
- Maintain code quality and test coverage
- Understand what AI agents are doing at all times
- Intervene when agents make wrong decisions

### Use Case 1: Full-Stack Feature Implementation

**Scenario:** Alex needs to implement "OAuth2 authentication with Google, GitHub, and Microsoft providers"

**Current Approach (Manual):**
1. Day 1-2: Research OAuth flows, setup Passport.js
2. Day 3-4: Implement backend routes and strategies
3. Day 5-6: Build frontend UI components
4. Day 7: Database migrations
5. Day 8-9: Write tests and fix bugs
6. Day 10: Integration testing, PR review

**With PAOS:**
1. **Hour 1:** Write PRD, run `(paos:start "oauth-prd.txt")`
2. **Hour 1-2:** Review task breakdown (4 parallel groups: backend, frontend, db, tests)
3. **Hour 2-6:** Monitor 4 agents working simultaneously
   - Backend agent: Passport strategies, routes, session handling
   - Frontend agent: OAuth buttons, login flows, error states
   - DB agent: Schema changes, migrations
   - Test agent: E2E and unit tests
4. **Hour 6-7:** Review PRs, resolve minor conflicts, merge
5. **Outcome:** Feature complete in 1 day vs. 10 days

### Use Case 2: Bug Fix Cascade

**Scenario:** Production bug: "Race condition in payment processing affects checkout, receipts, and email notifications"

**Challenge:** Bug spans 3 domains with shared state

**With PAOS:**
1. Write diagnostic PRD: "Fix race condition in payment flow"
2. System identifies affected areas: payment service, receipt generator, email queue
3. Spawns 3 specialized agents per domain
4. Agents work in parallel but orchestrator detects shared state (payment_id)
5. System suggests merge order: payment → receipts → email
6. Developer reviews, tests, ships fix in hours vs. days

### Use Case 3: Large-Scale Refactoring

**Scenario:** "Migrate REST API to GraphQL across 30 endpoints"

**With PAOS:**
1. PRD describes migration requirements
2. System breaks into 30 parallel tasks (one per endpoint)
3. Spawns 6 agents working on 5 endpoints each
4. Orchestrator ensures schema consistency
5. Agents generate resolvers, types, tests simultaneously
6. Developer reviews GraphQL schema, approves migrations
7. Weeks of work compressed to 2-3 days

---

## 5. Functional Requirements

### 5.1 Task Decomposition Engine

#### FR-1.1: PRD Parsing

**Description:** System ingests Product Requirements Document and generates structured task breakdown.

**Input:**
- Text-based PRD file (Markdown, plain text, or structured YAML)
- File path provided to `(paos:start "prd.txt")`

**Processing:**
- Invoke Taskmaster CLI: `task-master parse-prd prd.txt`
- Taskmaster uses Claude API to analyze requirements
- Generates 5-15 high-level tasks with:
  - Unique task IDs
  - Clear descriptions
  - Dependency relationships
  - Effort estimates (low/medium/high)
  - Initial tag assignments

**Output:**
- Structured YAML file: `.taskmaster/tasks/tasks.yaml`
- Tasks displayed in terminal for review

**Validation:**
- User review loop before agent spawning
- Edit/approve/reject workflow

**Example Output:**
```yaml
tasks:
  - id: 1
    description: "Implement OAuth backend routes and Passport strategies"
    tags: [backend, auth]
    dependencies: []
    effort: high
    
  - id: 2
    description: "Design and build OAuth UI components"
    tags: [frontend, ui]
    dependencies: []
    effort: medium
```

#### FR-1.2: Intelligent Tagging

**Description:** Assign domain-specific tags to enable parallel execution grouping.

**Capability:**
- Automatic tag assignment based on task content analysis
- Use Claude API with specialized prompt:
  ```
  "Analyze these tasks and assign parallelism tags:
   - 'ui' for frontend/visual components
   - 'backend' for API/server logic
   - 'db' for database/schema changes
   - 'tests' for test suites
   - 'docs' for documentation
   - 'infra' for deployment/config
   
   Tasks that can run simultaneously should share no dependencies.
   Output updated YAML."
  ```

**Logic:**
- Parse task descriptions for domain keywords
- Analyze dependency graph to identify parallel-safe groups
- Flag sequential dependencies (e.g., DB before backend)

**Customization:**
- User can override automatic tags via interactive prompt
- Command: `(paos:retag-task 3 'ui)` to change task 3's tag

**Output:**
- Updated `tasks-tagged.yaml` with parallel groups identified

#### FR-1.3: Subtask Expansion

**Description:** Decompose high-level tasks into atomic, executable subtasks.

**Trigger:**
- After user approves high-level task breakdown
- Before agent spawning

**Process:**
- For each high-level task, call Taskmaster expansion:
  ```bash
  task-master expand 1  # Expands task ID 1
  ```
- Claude generates 3-8 subtasks per task
- Subtasks are atomic (completable in single session)

**Depth:**
- Support 2-3 levels of hierarchy:
  - Task → Subtask → Micro-task (optional)
- Example:
  ```
  Task: "Implement OAuth backend"
    Subtask 1: "Setup Passport.js configuration"
      Micro-task 1.1: "Install passport and strategies"
      Micro-task 1.2: "Create config/passport.js"
    Subtask 2: "Create /auth routes"
    Subtask 3: "Implement session management"
  ```

**Output:**
- Expanded task trees saved to `.taskmaster/tasks/expanded/`
- Each tree assigned to one agent

---

### 5.2 Agent Orchestration

#### FR-2.1: Git Worktree Management

**Description:** Create isolated Git worktrees for each parallel agent group.

**Creation Process:**
1. For each tag group (e.g., 'ui', 'backend'), create worktree:
   ```bash
   git worktree add ../worktree-ui -b ui/oauth-implementation
   ```
2. Naming convention: `worktree-{tag}-{timestamp}`
3. Branch naming: `{tag}/{task-description-slug}`

**Environment Initialization:**
- Auto-detect project type (package.json → npm, requirements.txt → pip)
- Run setup commands:
  ```bash
  cd ../worktree-ui
  npm install  # or pip install, cargo build, etc.
  ```
- Copy configuration files (`.env.example` → `.env`)

**State Isolation:**
- Each worktree has independent file system
- Shared Git history and remote connections
- No cross-contamination between agents

**Cleanup:**
- Post-merge removal: `git worktree remove ../worktree-ui`
- Confirmation prompt before deletion
- Option to preserve for debugging

**Implementation (Common Lisp):**
```lisp
(defun create-worktree (tag base-path)
  "Create Git worktree for agent group"
  (let* ((timestamp (get-universal-time))
         (worktree-path (format nil "~A/worktree-~A-~A" 
                               base-path tag timestamp))
         (branch-name (format nil "~A/task-~A" tag timestamp)))
    (uiop:run-program 
      (list "git" "worktree" "add" worktree-path "-b" branch-name)
      :output :string
      :error-output :string)
    (init-project-environment worktree-path)
    worktree-path))
```

#### FR-2.2: Sub-Agent Spawning

**Description:** Launch Claude Code instances in Zellij tabs for each agent group.

**Mechanism:**
- Create named Zellij session: `paos-{project-name}-{session-id}`
- Tab 0: Orchestrator dashboard + REPL
- Tabs 1-N: Individual agent sessions

**Configuration Per Agent:**
- **Working Directory:** Set to worktree path
- **Context:** Pass task tree via:
  - Environment variable: `PAOS_TASK_TREE=/path/to/tasks.yaml`
  - Initial prompt file: `.paos/agent-context.txt`
- **Tools:** Define available operations:
  - Git: commit, push, status, diff
  - File: read, write, search, move
  - Test: run-tests, check-coverage
  - Status: report-progress, log-output

**Parallelism:**
- Support 2-8 simultaneous agents (configurable)
- Resource-aware spawning (check CPU/memory)
- Queue additional agents if system constrained

**Agent Prompt Template:**
```
You are a coding sub-agent for the PAOS system.

Your task tree:
{subtasks_yaml}

Working directory: {worktree_path}
Branch: {branch_name}

Tools available:
- git: commit, push, status (use absolute paths)
- file: read, write, search
- test: run-tests

After each significant step:
1. Write progress to: /shared/status-{tag}.json
2. Commit changes with descriptive message
3. Continue to next subtask

If blocked, report blocker in status file and wait.
```

**Zellij Integration:**
```lisp
(defun spawn-agent (tag subtasks worktree-path)
  "Launch Claude Code in Zellij tab"
  (let ((tab-name (format nil "~A Agent" (string-upcase tag))))
    ;; Create context file
    (write-agent-context worktree-path subtasks)
    
    ;; Create Zellij tab
    (uiop:run-program 
      (list "zellij" "action" "new-tab" 
            "--name" tab-name
            "--cwd" worktree-path
            "--" "claude" "--context" ".paos/agent-context.txt")
      :ignore-error-status t)
    
    ;; Initialize status file
    (write-initial-status tag subtasks worktree-path)))
```

#### FR-2.3: Agent-Computer Interface (ACI)

**Description:** Well-designed tool definitions following Anthropic best practices.

**Design Principles:**
1. **Cognitive Headroom:** Give model space to "think" before output
2. **Natural Formats:** Keep close to text seen on internet (avoid complex escaping)
3. **No Overhead:** Eliminate format requirements (line counting, JSON escaping)
4. **Clear Documentation:** Treat tool docs like junior developer onboarding
5. **Poka-Yoke:** Make incorrect usage difficult (e.g., require absolute paths)

**Example Tool Definition:**
```yaml
name: git_commit
description: |
  Commit changes to current branch with descriptive message.
  
  IMPORTANT: 
  - Always use absolute paths: /home/user/worktree-ui/src/App.tsx
  - Write clear, conventional commit messages
  - Commit frequently (after each logical change)
  
  Examples:
    ✓ git_commit("/home/.../LoginForm.tsx", "feat: add OAuth login form")
    ✗ git_commit("src/App.tsx", "changes")  # Relative path fails!

parameters:
  - file_path: string (absolute path to file)
  - message: string (conventional commit format)

returns:
  - commit_hash: string
  - status: success | error
```

**Tool Categories:**

**Git Operations:**
- `git_commit(file, message)` - Commit single file
- `git_push()` - Push current branch
- `git_status()` - Get working tree status
- `git_diff(file)` - Show uncommitted changes

**File Operations:**
- `file_read(path)` - Read file contents
- `file_write(path, content)` - Write/overwrite file
- `file_search(pattern, directory)` - Search for text
- `file_tree(directory)` - List directory structure

**Testing:**
- `run_tests(pattern)` - Run test suite (e.g., "*.test.js")
- `check_coverage()` - Get test coverage report

**Status Reporting:**
- `report_progress(completed, total, output)` - Update status file
- `log_blocker(description)` - Flag issue requiring human input

**Implementation:**
- Tools defined in `.paos/tools.yaml`
- Passed to Claude Code via `--tools` flag or MCP server
- Validated against schema before execution

---

### 5.3 Monitoring & Feedback

#### FR-3.1: Real-Time Status Dashboard

**Description:** Terminal-based UI showing all active agents and their progress.

**Display Components:**

**1. Header:**
```
=== PAOS Dashboard (14:32:01) ===
Session: paos-myproject-1729098721
Active Agents: 4/6  |  Total Progress: 62%
```

**2. Agent Summary (per agent):**
```
▸ UI Agent          [████████░░] 80% (4/5 tasks)
  Last: "Created LoginForm.tsx with validation"
  Branch: ui/oauth-implementation (3 commits)
  Status: WORKING
  
▸ Backend Agent     [█████░░░░░] 50% (2/4 tasks)  ⚠️
  Last: "Implemented Passport strategies"
  Branch: backend/oauth-implementation (2 commits)
  Blocker: "Test failing in auth.test.js"
  Status: BLOCKED
```

**3. Orchestrator Insights:**
```
🤖 Orchestrator Analysis:
   - Backend agent blocked on test failure
   - Suggest: (focus 'backend) to debug
   - UI agent ahead; may need backend API ready
   - No conflicts detected yet
```

**4. Command Prompt:**
```
Commands: (status) (focus TAG) (pause TAG) (review)
REPL> _
```

**Update Frequency:**
- Dashboard refreshes every 2-5 seconds
- Uses ANSI escape codes for in-place updates (no scroll)
- Highlights changes (new commits, blockers) in yellow/red

**Implementation:**
```lisp
(defun display-dashboard (statuses)
  "Render dashboard with ANSI formatting"
  (clear-screen)
  (format t "~C[2J~C[H" #\Escape #\Escape)  ; Clear + home
  
  ;; Header
  (format t "~%~C[1;36m=== PAOS Dashboard (~A) ===~C[0m~%~%" 
          #\Escape (format-timestamp) #\Escape)
  
  ;; Agent summaries
  (loop for (tag . status) in statuses
        do (print-agent-summary tag status))
  
  ;; Orchestrator insights
  (when (orchestrator-has-feedback-p)
    (format t "~%~C[1;33m🤖 Orchestrator Analysis:~C[0m~%" 
            #\Escape #\Escape)
    (print-orchestrator-feedback))
  
  ;; Prompt
  (format t "~%REPL> ")
  (force-output))
```

#### FR-3.2: Agent Status Protocol

**Description:** Structured status reporting from agents to orchestrator.

**Mechanism:**
- Each agent writes JSON to shared directory: `/shared/status-{tag}.json`
- Orchestrator polls every 5-10 seconds
- Status includes progress, output, blockers, timestamps

**Status File Schema:**
```json
{
  "agent_id": "ui-1729098721",
  "tag": "ui",
  "session_start": "2025-10-16T14:00:00Z",
  "last_update": "2025-10-16T14:32:01Z",
  
  "progress": {
    "completed_subtasks": 4,
    "total_subtasks": 5,
    "percentage": 0.80
  },
  
  "current_task": {
    "id": "ui-5",
    "description": "Implement OAuth button components",
    "started_at": "2025-10-16T14:30:00Z"
  },
  
  "recent_output": [
    "Created LoginButton.tsx with Google provider",
    "Added GitHub provider styling",
    "Implemented loading states"
  ],
  
  "blockers": [],
  
  "git_status": {
    "branch": "ui/oauth-implementation",
    "commits": 3,
    "uncommitted_files": 1
  },
  
  "health": "WORKING"  // WORKING | BLOCKED | COMPLETE | ERROR
}
```

**Update Triggers:**
- After completing each subtask
- Before/after Git operations
- When encountering blocker
- Every 60 seconds (heartbeat)

**Polling Logic:**
```lisp
(defun poll-agent-statuses (tags)
  "Read all agent status files"
  (loop for tag in tags
        for status-file = (format nil "/shared/status-~A.json" tag)
        when (probe-file status-file)
        collect (cons tag (parse-json-file status-file))))
```

#### FR-3.3: Orchestrator Intelligence

**Description:** Top-level LLM analyzes agent statuses and provides guidance.

**Analysis Frequency:**
- Every 30-60 seconds
- Immediately after any blocker reported
- On-demand via `(status)` command

**Capabilities:**

**1. Progress Summarization:**
- Aggregate completion percentages
- Identify fast/slow agents
- Estimate time to completion

**2. Conflict Detection:**
- Scan commits across worktrees for overlapping files
- Check `package.json` / `requirements.txt` for dependency conflicts
- Identify API contract changes affecting multiple agents
- Alert before conflicts manifest

**3. Recommendation Engine:**
- Suggest next actions for developer
- Propose agent re-prioritization
- Recommend merge order based on dependencies

**4. Blocker Analysis:**
- Interpret blocker messages
- Suggest resolution steps
- Determine if human input required

**Example Prompts:**
```lisp
(defun orchestrator-analyze (all-statuses)
  "Use Claude to analyze system state"
  (let ((prompt (format nil 
    "You are the orchestrator for a parallel agent system.
     
     Current agent statuses:
     ~A
     
     Analyze and provide:
     1. Overall progress summary
     2. Any conflicts detected (file overlaps, dependencies)
     3. Recommended next actions for developer
     4. Suggested merge order if agents completing soon
     5. Any blockers requiring immediate attention
     
     Be concise. Output as structured text."
     (format-statuses all-statuses))))
    
    (claude-api-call prompt :model "claude-sonnet-4.5")))
```

**Output Display:**
- Printed in dashboard "Orchestrator Analysis" section
- Logged to `.paos/orchestrator.log`
- Critical alerts highlighted in red

#### FR-3.4: Conflict Detection

**Description:** Proactive identification of potential merge conflicts.

**Detection Methods:**

**1. File-Level Analysis:**
```lisp
(defun detect-file-conflicts (worktree-paths)
  "Check for overlapping file modifications"
  (let ((all-modified-files 
         (mapcar #'git-modified-files worktree-paths)))
    (find-overlaps all-modified-files)))
```

**2. Dependency Analysis:**
- Parse `package.json` from each worktree
- Compare dependency versions
- Flag discrepancies:
  ```
  ⚠️ Conflict Detected:
     UI Agent: Added "react-oauth2@1.2.0"
     Backend Agent: Added "react-oauth2@1.3.0"
     
     Suggest: Align versions before merge
  ```

**3. API Contract Changes:**
- Detect changes to API routes, function signatures
- Cross-reference with other agents' usage
- Example:
  ```
  ⚠️ Potential Conflict:
     Backend Agent: Changed /auth/callback signature
     Frontend Agent: Still using old signature in AuthFlow.tsx
     
     Action: Ensure frontend updated before merge
  ```

**Alert Levels:**
- **Info:** Overlapping areas, no immediate conflict
- **Warning:** Likely conflict if merged now
- **Critical:** Guaranteed conflict, requires resolution

**Resolution Workflow:**
- Orchestrator suggests merge order (e.g., backend before frontend)
- Developer can pause agents to sync
- Manual conflict resolution if needed

---

### 5.4 Human-in-the-Loop Interaction

#### FR-4.1: Direct Agent Communication

**Description:** Developer can interact with any agent directly via Zellij tabs.

**Tab Navigation:**
- **List Tabs:** `Ctrl+t` shows all agent tabs
- **Switch by Number:** `Ctrl+t` → `2` switches to tab 2
- **Switch by Name:** `Ctrl+t` → type "Backend" → Enter

**Agent Interaction:**
- Once focused on agent tab, developer has full Claude Code CLI
- Chat with agent: type messages as normal
- Approve/reject suggestions: standard Claude Code workflow
- View agent's file operations in real-time

**Custom Commands:**
Agents respond to special commands in chat:
- `pause` - Halt current work, wait for instructions
- `status` - Print detailed status to terminal
- `skip-task` - Move to next subtask
- `retry` - Retry last failed operation

**Example Interaction:**
```
Developer switches to Backend Agent tab:

  Backend Agent Tab
  ─────────────────────────────────────
  ~/worktree-backend $ claude
  
  Claude: I've implemented the auth endpoints, but test suite failing.
  
  Test Output:
    ✗ POST /auth/google returns 302
    ✗ Session should contain user_id
  
  Issue appears to be missing session middleware. Shall I add 
  express-session configuration?
  
  Developer: Yes, use secure defaults with Redis store
  
  Claude: Adding express-session with Redis configuration...
  ✓ Created config/session.js
  ✓ Updated app.js to use session middleware
  ✓ Tests now passing
  
  Shall I commit these changes?
  
  Developer: Yes, then continue to next task
  
  Claude: Committed. Moving to OAuth provider configuration...
```

#### FR-4.2: Orchestrator Interface

**Description:** Main terminal providing dashboard and command interface.

**Layout:**
- Top 80%: Dashboard (auto-refreshing)
- Bottom 20%: REPL prompt for commands

**Available Commands:**

**Status & Information:**
- `(status)` - Full agent report with orchestrator analysis
- `(status 'backend)` - Detailed status for specific agent
- `(list-tasks)` - Show all tasks and completion state

**Agent Control:**
- `(focus TAG)` - Switch Zellij to agent's tab
  ```lisp
  (focus 'backend)  ; Switches to Backend Agent tab
  ```
- `(pause TAG)` - Halt specific agent
  ```lisp
  (pause 'ui)  ; UI agent stops after current subtask
  ```
- `(resume TAG)` - Restart paused agent
- `(pause-all)` - Halt all agents for system-wide sync

**Feedback & Guidance:**
- `(feedback TAG "message")` - Send instruction to agent
  ```lisp
  (feedback 'backend "Prioritize fixing failing tests")
  ```
- `(suggest)` - Ask orchestrator for recommendations

**Review & Merge:**
- `(review)` - Start PR review workflow
  ```lisp
  (review)
  ; Fetches all PRs, shows diffs, orchestrator analyzes
  ```
- `(merge TAG)` - Initiate merge for specific agent
  ```lisp
  (merge 'backend)
  ; Merges backend branch, runs post-merge tests, cleans worktree
  ```
- `(merge-all)` - Merge all completed agents in dependency order

**System Control:**
- `(save-session)` - Persist current state to disk
- `(quit)` - Gracefully shutdown all agents and exit

**Implementation:**
```lisp
(defun repl-command-loop ()
  "Main orchestrator REPL"
  (loop
    (handler-case
        (let ((cmd (read)))
          (case (car cmd)
            (status (print-status (cadr cmd)))
            (focus (focus-agent (cadr cmd)))
            (pause (pause-agent (cadr cmd)))
            (feedback (send-feedback (cadr cmd) (caddr cmd)))
            (review (review-prs))
            (merge (merge-agent (cadr cmd)))
            (quit (return-from repl-command-loop))
            (t (format t "Unknown command: ~A~%" cmd))))
      (error (e) 
        (format t "Error: ~A~%" e)))))
```

#### FR-4.3: Checkpoint System

**Description:** Automatic pause points for human review.

**Checkpoint Types:**

**1. Pre-Commit Checkpoints:**
- Agent completes file changes
- Prompts: "Review changes before commit? (yes/no/show)"
- If "show", displays diff
- Developer approves → agent commits

**2. Test Failure Checkpoints:**
- Agent runs tests, some fail
- Automatic pause with failure summary
- Developer can: debug, tell agent to retry, or skip tests

**3. Blocker Checkpoints:**
- Agent encounters unresolvable issue
- Reports blocker, awaits guidance
- Example: "API endpoint not found (needs backend merge first)"

**4. Merge Readiness Checkpoints:**
- Agent completes all subtasks
- Before creating PR, pauses for review
- Developer can request final changes

**Configuration:**
```yaml
# .paos/checkpoints.yaml
pre_commit: true          # Review before every commit
test_failures: true       # Pause on failing tests
blockers: true            # Always pause on blockers
merge_ready: true         # Review before PR creation
completion: true          # Confirm before merge to main

# Optional: Skip checkpoints for trusted agents
skip_checkpoints_for:
  - tests                 # Test agent commits freely
```

**Notification:**
- Checkpoint triggers orchestrator alert
- Dashboard highlights agent in yellow: "⏸️ CHECKPOINT"
- Developer sees: "Backend Agent awaiting approval for commit"

---

### 5.5 Integration & Completion

#### FR-5.1: Pull Request Management

**Description:** Automated PR creation with rich context.

**PR Creation Trigger:**
- Agent completes all subtasks
- Passes checkpoint review
- Commands: `(create-pr 'backend)` or agent auto-creates if configured

**PR Content:**

**Title Format:**
```
[{tag}] {High-level task description}

Example: [backend] Implement OAuth2 authentication with Passport.js
```

**Body Template:**
```markdown
## Summary
{High-level task description from task tree}

## Subtasks Completed
- [x] Setup Passport.js with Google strategy
- [x] Add GitHub and Microsoft strategies
- [x] Create /auth/callback endpoints
- [x] Implement session management

## Changes
- Files modified: 12
- Files added: 5
- Lines changed: +437 / -23

## Test Results
✓ All tests passing (47/47)
✓ Coverage: 92.3%

## Agent Notes
{Last 5 agent outputs}

## Related PRs
- Depends on: #41 (database schema changes)
- Blocks: #43 (frontend OAuth UI)
```

**Implementation:**
```lisp
(defun create-pr (tag)
  "Create GitHub PR for agent's work"
  (let* ((branch (agent-branch tag))
         (task (get-agent-task tag))
         (body (generate-pr-body tag)))
    (uiop:run-program 
      (list "gh" "pr" "create"
            "--title" (format nil "[~A] ~A" tag (task-description task))
            "--body" body
            "--base" "main"
            "--head" branch)
      :output :string)))
```

**GitHub CLI Integration:**
- Requires `gh` CLI authenticated
- Sets labels: `automated`, `paos`, `{tag}`
- Assigns to developer (from git config)
- Links to related PRs if dependencies exist

#### FR-5.2: Review Assistance

**Description:** Orchestrator helps developer review multiple PRs.

**Review Workflow:**

**1. Fetch PRs:**
```lisp
(review)

; Output:
; === Open PRs (4) ===
; #42 [backend] OAuth implementation (3 days ago)
; #43 [frontend] Login UI components (3 days ago)  
; #44 [db] User provider schema (3 days ago)
; #45 [tests] E2E OAuth tests (3 days ago)
```

**2. Orchestrator Analysis:**
- Fetches diffs for all PRs
- Uses Claude to analyze:
  ```
  "Review these PRs for:
   1. Code quality issues
   2. Potential bugs or edge cases
   3. Conflicts between PRs
   4. Suggested merge order
   5. Security concerns
   
   PRs:
   {pr_diffs}
  ```

**3. Summary Report:**
```
🤖 PR Review Summary:

#42 [backend]: Looks good overall
  ✓ Clean code, good error handling
  ⚠️ Consider rate limiting on /auth endpoints
  
#43 [frontend]: Minor issues
  ⚠️ Missing loading state for GitHub login
  ⚠️ Error messages could be more user-friendly
  
#44 [db]: Ready to merge
  ✓ Migration scripts well-structured
  ✓ Rollback tested
  
#45 [tests]: Comprehensive
  ✓ Good coverage of edge cases
  ✓ All tests passing

Suggested Merge Order:
  1. #44 (db) - no dependencies
  2. #42 (backend) - depends on db
  3. #43 (frontend) - depends on backend
  4. #45 (tests) - run after all merged

Critical Issues: None
Recommended Action: Merge in suggested order
```

**4. Developer Action:**
- Review flagged issues in GitHub
- Make manual edits if needed
- Approve PRs via `(approve-pr 42)` or `gh pr review`

#### FR-5.3: Merge Coordination

**Description:** Orchestrated merging with dependency awareness.

**Merge Sequence:**

**1. Dependency Analysis:**
```lisp
(defun merge-all)
  ; Orchestrator analyzes task dependencies
  ; Determines safe merge order: db → backend → frontend → tests
```

**2. Sequential Merging:**
```
Merging #44 (db)...
  ✓ Merged to main
  ✓ Post-merge tests passed
  ✓ Worktree cleaned: ../worktree-db

Merging #42 (backend)...
  ✓ Merged to main
  ⚠️ Integration test failed: auth.integration.test.js
  
  Action Required: Fix integration test before continuing
  
  Developer: (fix-merge-issue 42)
  ; Opens main branch, runs tests, investigates
  
  Developer: (retry-merge 42)
  ✓ Tests passing after fix
  ✓ Merge completed
```

**3. Post-Merge Validation:**
- Run full test suite on main branch
- Check for integration issues
- Generate merge report

**4. Cleanup:**
```
All PRs merged successfully!

Cleaned worktrees:
  ✓ ../worktree-db
  ✓ ../worktree-backend
  ✓ ../worktree-frontend
  ✓ ../worktree-tests

Session Summary:
  Total Tasks: 4
  Parallel Agents: 4
  Total Time: 6.5 hours
  Commits: 47
  Files Changed: 89
  
Saved to: .paos/sessions/session-1729098721.log
```

**Rollback Support:**
```lisp
(defun rollback-merge (pr-number)
  "Revert merge if integration issues found"
  (uiop:run-program 
    (list "git" "revert" "-m" "1" (pr-merge-commit pr-number))))
```

---

## 6. Non-Functional Requirements

### 6.1 Performance

| Requirement | Target | Measurement |
|------------|--------|-------------|
| **System Startup** | <5 seconds from `(paos:start)` to task display | Time to first interaction |
| **Agent Spawn Time** | <10 seconds per agent (worktree + Claude Code launch) | Spawn completion timestamp |
| **Dashboard Refresh** | 2-5 second update cycle | Frame time |
| **Orchestrator Overhead** | <5% CPU usage for monitoring loop | System resource monitor |
| **Agent Parallelism** | Support 8 agents on typical dev laptop (16GB RAM, 8 cores) | Stress testing |
| **Status File I/O** | <100ms read/write latency | I/O benchmarks |
| **LLM API Latency** | <3 seconds for orchestrator analysis | API response time |

**Optimization Strategies:**
- Batch status file reads in single loop
- Cache orchestrator analysis for 30s
- Lazy-load agent logs (only when requested)
- Async Zellij commands (don't block REPL)

### 6.2 Reliability

| Requirement | Target | Recovery Mechanism |
|------------|--------|-------------------|
| **Fault Tolerance** | Orchestrator survives single agent crash | Agent process monitoring + restart |
| **State Persistence** | All task state saved; recoverable after restart | Auto-save to `.paos/session-{id}.lisp` every 60s |
| **Error Recovery** | Agents retry failed operations 2-3 times | Exponential backoff on API errors |
| **Session Recovery** | Resume after system crash/reboot | `(paos:resume-session)` from saved state |
| **Data Loss Prevention** | Zero loss of committed work | Git worktrees persist until explicit cleanup |
| **API Failure Handling** | Graceful degradation if Claude API down | Queue requests, notify developer, continue local work |

**Backup & Recovery:**
```lisp
(defun auto-save-session ()
  "Persist session state every 60 seconds"
  (bordeaux-threads:make-thread
    (lambda ()
      (loop
        (sleep 60)
        (save-session-to-disk *current-session*)))))

(defun resume-session (&optional session-file)
  "Restore from saved session"
  (let ((state (load-session-from-disk session-file)))
    (restore-worktrees state)
    (reattach-zellij-session (session-zellij-name state))
    (setf *current-session* state)
    (format t "Session resumed: ~A agents active~%" 
            (length (session-agents state)))))
```

### 6.3 Usability

| Requirement | Target | Validation |
|------------|--------|-----------|
| **Learning Curve** | Developer productive within 30 minutes | User onboarding study |
| **Command Discoverability** | All commands shown in help: `(help)` | Help command completeness |
| **Error Messages** | Clear, actionable error descriptions | User feedback |
| **Dashboard Clarity** | Information density without clutter | UX review |
| **Keyboard Navigation** | All operations accessible via keyboard | Accessibility audit |
| **Documentation** | Comprehensive README + inline help | Doc coverage metrics |

**UX Principles:**
- **Progressive Disclosure:** Simple commands first, advanced via `(help 'advanced)`
- **Sensible Defaults:** Zero-config startup for standard projects
- **Visual Hierarchy:** Color-coded status (green=working, yellow=warning, red=error)
- **Graceful Degradation:** If Zellij unavailable, fallback to simple terminal output

**Help System:**
```lisp
(defun help (&optional topic)
  "Display help for PAOS commands"
  (case topic
    (nil (print-basic-help))
    (commands (print-command-reference))
    (advanced (print-advanced-features))
    (troubleshooting (print-common-issues))
    (t (format t "Unknown help topic: ~A~%" topic))))
```

### 6.4 Maintainability

| Requirement | Standard | Implementation |
|------------|----------|----------------|
| **Code Clarity** | Prefer simple over clever | Code review checklist |
| **Documentation** | Inline comments for all prompts and tools | Doc coverage tool |
| **Modularity** | Independent testing of components | Unit test suite |
| **Logging** | Comprehensive logs at debug level | `.paos/logs/` directory |
| **Configuration** | YAML-based config, no hardcoded values | Config file validation |
| **Versioning** | Semantic versioning, changelog maintained | Release process |

**Code Organization:**
```
paos/
├── core/
│   ├── orchestrator.lisp      ; Main coordinator
│   ├── decomposer.lisp         ; Task breakdown logic
│   ├── spawner.lisp            ; Agent/worktree management
│   └── monitor.lisp            ; Status polling & dashboard
├── agents/
│   ├── prompts.lisp            ; Agent prompt templates
│   └── tools.lisp              ; Tool definitions
├── integrations/
│   ├── taskmaster.lisp         ; Taskmaster CLI wrapper
│   ├── zellij.lisp             ; Zellij session management
│   └── github.lisp             ; PR creation & review
├── ui/
│   └── dashboard.lisp          ; Terminal UI rendering
├── utils/
│   ├── git.lisp                ; Git operations
│   ├── file-io.lisp            ; Status file handling
│   └── api.lisp                ; Claude API client
└── paos.asd                    ; ASDF system definition
```

**Testing Strategy:**
- **Unit Tests:** Each module independently testable (e.g., `test-spawner.lisp`)
- **Integration Tests:** End-to-end with mock agents
- **Smoke Tests:** Real PRD examples from `examples/` directory
- **Dogfooding:** Use PAOS to build PAOS features

### 6.5 Security

| Requirement | Implementation | Threat Mitigated |
|------------|----------------|------------------|
| **API Key Storage** | Environment variables or encrypted config | Key exposure |
| **Sandbox Isolation** | Agents operate in worktrees, no direct main branch access | Accidental main corruption |
| **Audit Trail** | All agent actions logged with timestamps | Accountability |
| **Code Review Required** | No auto-merge to main; human approval mandatory | Malicious/buggy code |
| **Rate Limiting** | Configurable API call limits per session | Cost overruns |
| **Credential Handling** | Never pass secrets to LLMs | Secret leakage |

**Security Best Practices:**
```lisp
(defun load-api-key ()
  "Safely load Anthropic API key"
  (or (uiop:getenv "ANTHROPIC_API_KEY")
      (read-encrypted-config ".paos/config.enc" "api_key")
      (error "API key not found. Set ANTHROPIC_API_KEY or run (configure-api-key)")))

(defun sanitize-prompt (text)
  "Remove potential secrets before sending to LLM"
  (let ((patterns '("api[_-]key" "password" "token" "secret")))
    (dolist (pattern patterns)
      (setf text (ppcre:regex-replace-all 
                   (format nil "~A\\s*[:=]\\s*\\S+" pattern)
                   text
                   (format nil "~A=***REDACTED***" pattern)))))
  text)
```

**Worktree Isolation Benefits:**
- Agents cannot accidentally push to main
- Merge requires explicit developer action
- Easy rollback (delete worktree)
- No risk of cross-agent interference

---

## 7. Technical Architecture & Language Choice

### 7.1 Language Decision: Common Lisp

#### Rationale

**Primary Language:** Common Lisp (SBCL implementation)

**Why Common Lisp is Ideal:**

**1. Rapid Prototyping & Iteration:**
- **REPL-Driven Development:** Modify running system without restart
- **Interactive Debugging:** Inspect live agent state, fix bugs on-the-fly
- **Fast Feedback Loop:** Test prompt changes in seconds

**2. Flexibility & Control:**
- **Macros:** Build domain-specific language for task definitions without framework overhead
- **Minimal Magic:** See exactly what's happening—no hidden abstraction layers
- **Lisp Philosophy:** Aligns with "simple, composable patterns" from Anthropic guidance

**3. Concurrency:**
- **Bordeaux Threads:** Portable threading for agent management
- **No GIL:** True parallel execution for monitoring loop
- **Green Threads:** SBCL supports lightweight processes

**4. Performance:**
- **Native Compilation:** SBCL compiles to machine code (faster than Python)
- **Efficient I/O:** Fast file/network operations for status polling
- **Low Overhead:** Orchestrator uses minimal resources

**5. Stability & Maturity:**
- **40+ Years Old:** Battle-tested, stable language
- **Rich Ecosystem:** Quicklisp for dependencies (cl-json, dexador, etc.)
- **Long-Lived Processes:** Perfect for persistent orchestrator sessions
- **Minimal Dependency Churn:** Libraries rarely break compatibility

**6. Developer Experience:**
- **"Old School, Hardcore":** Matches desired aesthetic
- **Powerful Introspection:** Inspect/modify any part of running system
- **Expressive:** Write less code than Go/Java for same functionality

#### Comparison to Alternatives

| Language | Pros | Cons | Verdict |
|----------|------|------|---------|
| **Python** | Large AI ecosystem, popular | GIL bottleneck, framework temptations, slower | ❌ Too slow, concurrency issues |
| **Elixir** | Excellent concurrency (OTP), functional | Smaller AI tooling, unfamiliar to most | ⚠️ Great choice, but niche |
| **Go** | Good performance, simple concurrency | Verbose, no REPL, less flexible | ⚠️ Solid but lacks REPL iteration |
| **Rust** | Best performance, memory safety | Steep learning curve, slow prototyping | ❌ Painful for experimentation |
| **Common Lisp** | REPL, macros, concurrency, stable | Smaller community, fewer libs | ✅ **Best fit** |

**Hybrid Approach:**
- **Core Orchestrator:** Common Lisp (decomposition, spawning, monitoring)
- **Sub-Agents:** Claude Code CLI (Node.js-based)—don't reinvent
- **Taskmaster Integration:** Shell out to Node CLI via `uiop:run-program`

#### Why Not Elixir?

Elixir would also be excellent for this project:
- **OTP Supervision Trees:** Perfect for agent lifecycle management
- **Actor Model:** Natural fit for parallel agents
- **Phoenix LiveView:** Could add web UI easily later

**Decision:** Use Lisp for:
- Better REPL for prompt engineering
- Simpler single-binary distribution (SBCL)
- "Hardcore" aesthetic preference
- Smaller dependency footprint

**Alternative:** If team strongly prefers Elixir, re-implement in Phase 2

### 7.2 Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                    Main Terminal (Lisp REPL)                 │
│  ┌────────────────────────────────────────────────────────┐  │
│  │     Orchestrator Agent (Claude Sonnet 4.5)             │  │
│  │  - Task decomposition (via Taskmaster CLI)             │  │
│  │  - Agent spawning & lifecycle management               │  │
│  │  - Conflict detection & feedback                       │  │
│  │  - PR creation & merge coordination                    │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐  │
│  │            Status Dashboard (Auto-refresh)             │  │
│  │  - Agent progress bars                                 │  │
│  │  - Recent logs & blockers                              │  │
│  │  - Orchestrator insights                               │  │
│  └────────────────────────────────────────────────────────┘  │
└────────────────────────┬─────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ Zellij Tab 1 │ │ Zellij Tab 2 │ │ Zellij Tab N │
│              │ │              │ │              │
│ Worktree:    │ │ Worktree:    │ │ Worktree:    │
│ ../wt-ui     │ │ ../wt-backend│ │ ../wt-db     │
│ Branch:      │ │ Branch:      │ │ Branch:      │
│ ui/oauth     │ │ backend/oauth│ │ db/schema    │
│              │ │              │ │              │
│ Claude Code  │ │ Claude Code  │ │ Claude Code  │
│ Instance     │ │ Instance     │ │ Instance     │
│              │ │              │ │              │
│ $ claude     │ │ $ claude     │ │ $ claude     │
└──────┬───────┘ └──────┬───────┘ └──────┬───────┘
       │                │                │
       └────────────────┴────────────────┘
                        │
              ┌─────────▼──────────┐
              │  Shared State Dir  │
              │  /tmp/paos-state/  │
              │                    │
              │  - status-ui.json  │
              │  - status-back.json│
              │  - status-db.json  │
              │  - tasks.yaml      │
              │  - orchestrator.log│
              └────────────────────┘
```

### 7.3 Component Details

#### 7.3.1 Orchestrator Core

**File:** `core/orchestrator.lisp`

**Responsibilities:**
- Session initialization and lifecycle
- Task decomposition coordination
- Agent spawning and monitoring
- Command processing (REPL loop)
- Status aggregation and analysis

**Key Functions:**
```lisp
(defun start (prd-file)
  "Entry point: parse PRD and begin orchestration")

(defun approve-tasks ()
  "User approves task breakdown; spawns agents")

(defun monitoring-loop (agent-tags)
  "Main monitoring loop: poll statuses, update dashboard, call orchestrator LLM")

(defun shutdown ()
  "Gracefully stop all agents, save session, cleanup")
```

#### 7.3.2 Task Decomposer

**File:** `core/decomposer.lisp`

**Responsibilities:**
- Invoke Taskmaster CLI for PRD parsing
- Tag assignment via Claude API
- Subtask expansion
- Task tree validation

**Key Functions:**
```lisp
(defun decompose-prd (prd-file)
  "Parse PRD using Taskmaster, return structured tasks")

(defun assign-parallel-tags (tasks prd-context)
  "Use Claude to tag tasks for parallelism")

(defun expand-subtasks (task)
  "Expand high-level task into atomic subtasks")

(defun validate-task-tree (tasks)
  "Check for circular dependencies, invalid tags")
```

#### 7.3.3 Agent Spawner

**File:** `core/spawner.lisp`

**Responsibilities:**
- Git worktree creation and cleanup
- Project environment initialization
- Zellij tab management
- Agent context file generation
- Status file initialization

**Key Functions:**
```lisp
(defun spawn-agent (tag subtasks worktree-path)
  "Create worktree, launch Claude Code in Zellij tab")

(defun create-worktree (tag base-path)
  "Create isolated Git worktree for agent")

(defun init-project-environment (worktree-path)
  "Run npm install / pip install / etc. in worktree")

(defun write-agent-context (worktree-path subtasks)
  "Create .paos/agent-context.txt with task instructions")

(defun cleanup-worktree (worktree-path)
  "Remove worktree after merge")
```

#### 7.3.4 Monitor & Dashboard

**File:** `core/monitor.lisp`, `ui/dashboard.lisp`

**Responsibilities:**
- Status file polling
- Dashboard rendering (ANSI terminal)
- Orchestrator LLM analysis
- Conflict detection
- Alert generation

**Key Functions:**
```lisp
(defun poll-agent-statuses (tags)
  "Read all agent status JSON files")

(defun display-dashboard (statuses)
  "Render terminal dashboard with progress bars")

(defun orchestrator-analyze (statuses)
  "Call Claude API to analyze agent states")

(defun detect-conflicts (worktree-paths)
  "Check for overlapping file changes or dep conflicts")
```

### 7.4 Data Flow

```
1. PRD Input
   └─> prd.txt written by developer

2. Decomposition Phase
   └─> task-master parse-prd prd.txt
   └─> .taskmaster/tasks/tasks.yaml generated
   └─> Claude API call for tagging
   └─> tasks-tagged.yaml saved

3. User Review
   └─> Display in terminal
   └─> Developer edits/approves
   └─> tasks-approved.yaml final version

4. Spawning Phase
   └─> For each tag group:
       ├─> git worktree add
       ├─> npm install (or equivalent)
       ├─> zellij action new-tab
       └─> claude launched with context

5. Execution Phase
   └─> Agents work independently
   └─> Every N operations:
       └─> Write to /tmp/paos-state/status-{tag}.json

6. Monitoring Phase
   └─> Orchestrator polls status files (every 5s)
   └─> Claude API analyzes statuses (every 30s)
   └─> Dashboard updates in terminal
   └─> Alerts shown for blockers/conflicts

7. Completion Phase
   └─> Agents create PRs via gh CLI
   └─> Developer reviews with orchestrator assistance
   └─> Merge in dependency order
   └─> Cleanup worktrees
   └─> Final report generated
```

### 7.5 Dependencies

#### Lisp Libraries (via Quicklisp)

```lisp
;; paos.asd
(defsystem "paos"
  :depends-on ("cl-json"           ; JSON parsing for status files
               "dexador"            ; HTTP client for Anthropic API
               "bordeaux-threads"   ; Portable threading
               "uiop"               ; Shell commands, file I/O
               "cl-yaml"            ; YAML parsing for Taskmaster output
               "cl-ppcre"           ; Regex for text processing
               "local-time"         ; Timestamp handling
               "alexandria"         ; Utility functions
               "str")               ; String manipulation
  :components ((:module "core" ...)
               (:module "agents" ...)
               (:module "integrations" ...)
               (:module "ui" ...)
               (:module "utils" ...)))
```

#### External Tools

**Required:**
- `sbcl` (Steel Bank Common Lisp) - Lisp implementation
- `quicklisp` - Lisp package manager
- `git` - Version control (≥2.25 for worktree features)
- `zellij` - Terminal multiplexer (≥0.39)
- `task-master-ai` - Task decomposition (npm package)
- `claude` CLI - Claude Code for sub-agents

**Optional:**
- `gh` CLI - GitHub PR creation/review
- `emacs` or `vim` - For PRD editing (any editor works)

#### Installation Script

```bash
#!/bin/bash
# install-paos.sh

echo "Installing PAOS dependencies..."

# Check SBCL
if ! command -v sbcl &> /dev/null; then
    echo "Installing SBCL..."
    # macOS: brew install sbcl
    # Ubuntu: apt-get install sbcl
    # Arch: pacman -S sbcl
fi

# Install Quicklisp
if [ ! -d "$HOME/quicklisp" ]; then
    echo "Installing Quicklisp..."
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
    rm quicklisp.lisp
fi

# Install Zellij
if ! command -v zellij &> /dev/null; then
    echo "Installing Zellij..."
    cargo install zellij
    # or: brew install zellij
fi

# Install Taskmaster
if ! command -v task-master &> /dev/null; then
    echo "Installing Taskmaster..."
    npm install -g task-master-ai
fi

# Install Claude Code (if not already installed)
# See: https://docs.claude.com/claude-code

# Install PAOS
echo "Loading PAOS into Quicklisp local-projects..."
git clone https://github.com/your-org/paos.git ~/quicklisp/local-projects/paos

echo "Installation complete! Start with: sbcl --load paos-init.lisp"
```

#### Configuration

**File:** `.paos/config.yaml`

```yaml
# PAOS Configuration

# Anthropic API
anthropic:
  api_key: ${ANTHROPIC_API_KEY}  # From environment variable
  model: claude-sonnet-4.5
  max_tokens: 4000
  timeout: 30

# Agent Settings
agents:
  max_parallel: 6               # Maximum simultaneous agents
  spawn_delay: 2                # Seconds between spawns
  retry_attempts: 3
  retry_backoff: exponential

# Monitoring
monitoring:
  status_poll_interval: 5       # Seconds
  orchestrator_analysis_interval: 30
  dashboard_refresh: 2

# Checkpoints
checkpoints:
  pre_commit: true
  test_failures: true
  blockers: true
  merge_ready: true

# Git
git:
  worktree_base: ..             # Parent directory for worktrees
  branch_prefix: paos           # Branch names: paos/{tag}/{task}
  auto_cleanup: false           # Manual cleanup by default

# Zellij
zellij:
  session_prefix: paos
  layout_file: ~/.config/zellij/paos-layout.kdl

# Logging
logging:
  level: info                   # debug | info | warn | error
  directory: .paos/logs
  max_size: 10MB
```

### 7.6 Deployment & Distribution

**Development Setup:**
```bash
cd ~/projects/myapp
git clone https://github.com/your-org/paos.git .paos/paos-core
cd .paos/paos-core
./install-paos.sh

# Initialize for project
paos init

# Create .paos/ structure
mkdir -p .paos/{logs,sessions,tasks,tools}
cp config.example.yaml .paos/config.yaml
```

**Binary Distribution (Future):**
- Use `buildapp` to create standalone executable
- Single binary: `paos`
- Usage: `paos start prd.txt`

**Docker Support (Future):**
```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y sbcl git zellij npm
RUN npm install -g task-master-ai
COPY . /paos
RUN cd /paos && make install
ENTRYPOINT ["paos"]
```

---

## 8. User Workflows

### 8.1 Standard Feature Implementation

**Scenario:** Developer needs to implement "OAuth2 authentication with multiple providers"

**Detailed Steps:**

#### Step 1: Write PRD (5 minutes)

Create `prd.txt`:
```markdown
# Feature: Multi-Provider OAuth Authentication

## Overview
Add OAuth2 authentication supporting Google, GitHub, and Microsoft.

## Requirements
- Users can sign up/login via OAuth
- Store provider info in user profile
- Support account linking (multiple providers per user)
- Admin dashboard shows provider usage stats

## Technical Constraints
- Backend: Node.js/Express
- Frontend: React
- Database: PostgreSQL
- Use Passport.js for OAuth

## Success Criteria
- All providers functional
- <200ms login latency
- 100% test coverage for auth flows
```

#### Step 2: Initialize PAOS (1 minute)

```bash
$ sbcl --load paos-init.lisp
```

```lisp
* (paos:start "prd.txt")

Initializing PAOS session...
✓ Created Zellij session: paos-myproject-1729098721
✓ Parsing PRD with Taskmaster...
✓ Task breakdown complete (4 high-level tasks)

=== Task Breakdown ===

1. [backend] Implement OAuth backend routes and Passport strategies
   Effort: HIGH | Dependencies: None
   
2. [frontend] Design and build OAuth UI components
   Effort: MEDIUM | Dependencies: None
   
3. [db] Create database schema for OAuth providers
   Effort: LOW | Dependencies: None
   
4. [tests] E2E and unit tests for OAuth flows
   Effort: MEDIUM | Dependencies: 1, 2, 3

✓ Assigned parallelism tags
✓ Identified 3 parallel groups (backend, frontend, db) + 1 sequential (tests)

Review tasks? (y/n/edit)
```

#### Step 3: Review & Approve (5 minutes)

```lisp
* y

Expanding subtasks...
✓ Task 1 expanded (5 subtasks)
✓ Task 2 expanded (4 subtasks)
✓ Task 3 expanded (3 subtasks)
✓ Task 4 expanded (6 subtasks)

=== Expanded Task Trees ===

[backend] OAuth Backend (5 subtasks)
  1.1 Setup Passport.js configuration
  1.2 Implement Google OAuth strategy
  1.3 Implement GitHub OAuth strategy
  1.4 Implement Microsoft OAuth strategy
  1.5 Create /auth callback endpoints and session handling

[frontend] OAuth UI (4 subtasks)
  2.1 Create OAuthButton component
  2.2 Build provider selection screen
  2.3 Implement loading and error states
  2.4 Add provider icons and styling

[db] Database Schema (3 subtasks)
  3.1 Create oauth_providers table migration
  3.2 Update user model with provider associations
  3.3 Add indexes for performance

[tests] OAuth Testing (6 subtasks)
  4.1 Write unit tests for Passport strategies
  4.2 E2E test for Google login flow
  4.3 E2E test for GitHub login flow
  4.4 E2E test for Microsoft login flow
  4.5 Test account linking scenarios
  4.6 Test error handling and edge cases

Approve and spawn agents? (yes/no)
```

```lisp
* yes

Spawning agents...

✓ Created worktree: ../worktree-backend
✓ Initialized environment: npm install
✓ Spawned Backend Agent (Zellij Tab 1)

✓ Created worktree: ../worktree-frontend
✓ Initialized environment: npm install
✓ Spawned Frontend Agent (Zellij Tab 2)

✓ Created worktree: ../worktree-db
✓ Initialized environment: npm install
✓ Spawned DB Agent (Zellij Tab 3)

Test agent will spawn after dependencies complete.

Entering monitoring mode...
```

#### Step 4: Monitoring Phase (2-4 hours)

Dashboard auto-refreshes:

```
=== PAOS Dashboard (14:32:01) ===
Session: paos-myproject-1729098721
Active Agents: 3/4  |  Overall Progress: 47%

▸ Backend Agent     [██████░░░░] 60% (3/5 subtasks)
  Current: "Implementing Microsoft OAuth strategy"
  Last Output: "✓ Added Google and GitHub strategies"
  Branch: backend/oauth-implementation (7 commits)
  Status: WORKING
  
▸ Frontend Agent    [████████░░] 75% (3/4 subtasks)
  Current: "Adding provider icons and styling"
  Last Output: "✓ Created OAuthButton with loading states"
  Branch: frontend/oauth-ui (5 commits)
  Status: WORKING
  
▸ DB Agent          [██████████] 100% (3/3 subtasks)  ✓
  Last Output: "✓ All migrations complete and tested"
  Branch: db/oauth-schema (4 commits)
  Status: COMPLETE → PR #44 created

⏸️ Tests Agent      [░░░░░░░░░░] 0% (queued)
  Waiting for: Backend Agent, Frontend Agent
  Status: QUEUED

🤖 Orchestrator Analysis:
   - DB Agent finished! PR ready for review.
   - Backend Agent 60% complete, on track
   - Frontend Agent ahead of backend (good progress)
   - No conflicts detected
   - Estimate: 1.5 hours until all agents complete
   
   Suggestion: Review PR #44 (db) now, can merge independently

Commands: (status) (focus TAG) (review) (pause TAG)
REPL> _
```

**Developer Actions During Monitoring:**

**Action 1: Review completed PR**
```lisp
* (review)

=== Open PRs (1) ===
#44 [db] OAuth provider schema (15 minutes ago)

Fetching PR details...
✓ All migrations tested
✓ Rollback scripts verified
✓ No conflicts with main

🤖 Orchestrator Analysis:
  ✓ Clean schema design
  ✓ Proper indexes for performance
  ✓ Foreign keys correctly defined
  ⚠️ Consider adding 'provider_user_id' index
  
  Ready to merge: YES
  
Merge now? (y/n)
```

```lisp
* y

Merging #44...
✓ Merged to main
✓ Post-merge tests passed
✓ Worktree cleaned: ../worktree-db

1/4 agents complete.
```

**Action 2: Check on blocker**

Dashboard updates:
```
▸ Backend Agent     [███████░░░] 70% (3.5/5 subtasks)  ⚠️
  Current: "Testing Microsoft OAuth callback"
  Blocker: "Test failing: session not persisting"
  Status: BLOCKED (awaiting input)
```

```lisp
* (focus 'backend)

; Switches to Backend Agent Zellij tab

Backend Agent Terminal:
─────────────────────────────────────
~/worktree-backend $ claude

Claude: I've implemented Microsoft OAuth strategy, but the session
test is failing:

  ✗ POST /auth/microsoft/callback should persist session
    Expected session.user_id to be defined
    Received: undefined

I've checked the session middleware and it's configured correctly.
The issue might be that the test isn't setting up session properly.

Shall I:
1. Update test to use supertest-session
2. Add more session debugging
3. Something else?

Developer: Check if express-session is initialized before passport. 
Also, ensure session secret is set in test env.

Claude: Good catch! Checking...

The issue is app.js initializes passport before express-session:
  Line 15: app.use(passport.initialize())
  Line 18: app.use(session(...))

This causes passport to not have access to session. Fixing order...

✓ Moved session middleware before passport
✓ Added SESSION_SECRET to .env.test
✓ Tests now passing!

Shall I commit these fixes?

Developer: Yes, continue

Claude: ✓ Committed: "fix: initialize session before passport"
Resuming next subtask: Creating callback endpoints...

; Developer switches back to dashboard
```

```lisp
* (focus 'dashboard)  ; Back to Tab 0
```

#### Step 5: Completion & Review (30 minutes)

After ~3 hours, all agents finish:

```
=== PAOS Dashboard (17:45:33) ===

All agents complete! 🎉

✓ Backend Agent   [██████████] 100% → PR #45
✓ Frontend Agent  [██████████] 100% → PR #46
✓ DB Agent        [██████████] 100% → PR #44 (MERGED)
✓ Tests Agent     [██████████] 100% → PR #47

4 PRs created. Ready for review.

Commands: (review) (merge-all)
```

```lisp
* (review)

=== PR Review Summary ===

Fetching and analyzing PRs...

#45 [backend] OAuth backend implementation
  Files: 12 modified, 3 added
  Commits: 18
  Tests: 23/23 passing
  
  🤖 Analysis:
    ✓ Clean code, good error handling
    ✓ All three providers implemented correctly
    ⚠️ Consider rate limiting on /auth endpoints
    ⚠️ Add CSRF protection to callback routes
    
  Recommendation: APPROVE with suggestions

#46 [frontend] OAuth UI components
  Files: 8 modified, 5 added
  Commits: 12
  Tests: 15/15 passing
  
  🤖 Analysis:
    ✓ Accessible components (ARIA labels)
    ✓ Good loading and error states
    ✓ Responsive design
    ⚠️ Missing focus management for keyboard nav
    
  Recommendation: APPROVE with minor fix

#47 [tests] E2E OAuth test suite
  Files: 6 added
  Commits: 8
  Tests: 28/28 passing
  
  🤖 Analysis:
    ✓ Comprehensive coverage (Google, GitHub, Microsoft)
    ✓ Account linking tested
    ✓ Edge cases covered
    
  Recommendation: APPROVE

Suggested merge order:
  1. #45 (backend) - core functionality
  2. #46 (frontend) - depends on backend API
  3. #47 (tests) - validation after integration

Merge all in order? (yes/no/manual)
```

```lisp
* yes

Merging PRs in dependency order...

[1/3] Merging #45 (backend)...
  ✓ Merged to main
  ✓ Running integration tests...
  ✓ All tests passing (150/150)
  ✓ Worktree cleaned

[2/3] Merging #46 (frontend)...
  ✓ Merged to main
  ✓ Running integration tests...
  ✓ All tests passing (165/165)
  ✓ Worktree cleaned

[3/3] Merging #47 (tests)...
  ✓ Merged to main
  ✓ Running full test suite...
  ✓ All tests passing (193/193)
  ✓ Worktree cleaned

=== Session Complete ===

Feature: Multi-Provider OAuth Authentication
Status: MERGED TO MAIN ✓

Summary:
  Total Time: 3h 45m
  Parallel Agents: 4
  Tasks Completed: 18 subtasks
  Files Changed: 89 (+1847 lines / -234 lines)
  Commits: 42
  Tests: 193 (all passing)
  
  Manual Intervention: 1 (backend blocker)
  
Efficiency Gain: ~7.5x vs. sequential implementation
  (Estimated manual time: 28 hours → Actual: 3h 45m)

Session saved to: .paos/sessions/session-1729098721.log

Ready for deployment! 🚀
```

### 8.2 Interactive Debugging Workflow

**Scenario:** Agent encounters a blocker mid-execution

**Agent encounters error:**
```json
{
  "agent_id": "backend-1729098721",
  "health": "BLOCKED",
  "blockers": [
    {
      "description": "Test suite failing: auth.integration.test.js",
      "error_output": "TypeError: Cannot read property 'id' of undefined",
      "suggested_action": "Check user object structure in callback"
    }
  ]
}
```

**Dashboard alerts developer:**
```
⚠️ Backend Agent BLOCKED

Blocker: Test suite failing
Error: Cannot read property 'id' of undefined
Location: auth.integration.test.js:45

Suggested Actions:
  1. (focus 'backend) - Debug directly
  2. (feedback 'backend "...") - Send guidance
  3. (skip-task) - Move to next subtask

REPL> _
```

**Developer debugs:**
```lisp
* (focus 'backend)

; Zellij switches to Backend Agent tab

Backend Agent Terminal:
─────────────────────────────────────
~/worktree-backend $ claude

Claude: I'm encountering a test failure. The integration test for
OAuth callback is failing with:

  TypeError: Cannot read property 'id' of undefined
  at /auth/callback (auth.js:87)

I've checked the code and the issue seems to be that `req.user` is
undefined when the test calls the callback endpoint. This suggests
the authentication middleware isn't running.

Would you like me to:
1. Add middleware to the callback route
2. Check test setup for authentication
3. Review the Passport configuration

Developer: Let's check the test setup. Show me auth.integration.test.js

Claude: [displays file]

Developer: The test isn't using an authenticated session. Add
`agent(app).post('/auth/callback').set('Cookie', loginCookie)` where
loginCookie comes from a prior login step.

Claude: Excellent point! I'll:
1. Create a test helper for login
2. Use session cookie in callback test
3. Ensure proper cleanup

Implementing...

✓ Created test/helpers/auth-helper.js
✓ Updated auth.integration.test.js to use authenticated session
✓ All tests now passing!

Shall I commit these fixes?

Developer: Yes, and continue with next subtask

Claude: ✓ Committed: "test: fix auth callback test with proper session"
Moving to next subtask: Implementing rate limiting...
```

### 8.3 Mid-Flight Adjustment Workflow

**Scenario:** Orchestrator detects UI agent is ahead; backend lagging

**Dashboard shows:**
```
🤖 Orchestrator Analysis:
   
   ⚠️ Timing Concern Detected
   
   - Frontend Agent: 80% complete
   - Backend Agent: 45% complete
   
   Frontend is implementing API calls that backend hasn't created yet.
   Risk: Frontend may need rework once backend API finalized.
   
   Suggestions:
   1. Pause frontend agent: (pause 'frontend)
   2. Prioritize backend: (feedback 'backend "Focus on API routes first")
   3. Continue as-is (frontend work may need adjustment)
   
   Recommended: Pause frontend, let backend catch up
```

**Developer acts:**
```lisp
* (pause 'frontend)

Pausing Frontend Agent...
✓ Agent will complete current subtask then pause
✓ Expected pause time: ~2 minutes

* (feedback 'backend "Prioritize completing all API routes before tests. We want to give frontend a stable API to work with.")

Feedback sent to Backend Agent.
```

**Backend agent receives feedback:**
```
Backend Agent:

Developer Feedback:
"Prioritize completing all API routes before tests. We want to give 
frontend a stable API to work with."

Claude: Understood. I'll focus on:
  1. Finishing all OAuth callback routes
  2. Defining API contract (response schemas)
  3. Basic smoke tests for each endpoint
  
Then I'll update API documentation before resuming comprehensive tests.

Adjusting task priority...
```

**Later, backend catches up:**
```
Backend Agent: 75% complete (API routes finished)

* (resume 'frontend)

Resuming Frontend Agent...
✓ Agent has latest backend code
✓ Continuing from: "Connect OAuth buttons to API"
```

---

## 9. Edge Cases & Error Handling

### 9.1 System Failures

| Scenario | Detection | Recovery Mechanism | User Experience |
|----------|-----------|-------------------|-----------------|
| **Agent Crash** | Status file not updated for 2+ minutes | Orchestrator detects timeout; prompts to restart agent | "⚠️ Backend Agent unresponsive. Restart? (y/n)" |
| **Orchestrator Crash** | N/A (system-level) | Auto-save session every 60s; `(paos:resume-session)` on restart | "Resuming from last checkpoint (2 min ago)" |
| **Zellij Crash** | Zellij process exits | Orchestrator re-creates session; reattaches agents | "Re-initializing Zellij session..." |
| **Git Operations Fail** | Non-zero exit code from git commands | Retry 3x with backoff; escalate to developer if persist | "Git worktree creation failed. Retry? (y/n/debug)" |
| **Network Failure (API)** | API call timeout/error | Queue API calls; retry; agents continue local work | "⚠️ API temporarily unavailable. Queuing requests..." |
| **Disk Full** | Write errors on status files | Alert immediately; suggest cleanup; pause agents | "🚨 Disk full! Clear space or risk data loss." |

**Implementation Example:**
```lisp
(defun agent-health-check (tag last-update)
  "Detect unresponsive agents"
  (let ((elapsed (- (get-universal-time) last-update)))
    (cond
      ((> elapsed 300) ; 5 minutes
       (alert-developer tag :critical "Agent unresponsive >5min"))
      ((> elapsed 120) ; 2 minutes
       (alert-developer tag :warning "Agent slow to respond")))))

(defun restart-agent (tag)
  "Restart crashed agent, preserving state"
  (let ((worktree (agent-worktree tag))
        (last-status (load-last-status tag)))
    (format t "Restarting ~A Agent from last known state...~%" tag)
    (spawn-agent tag 
                 (status-remaining-subtasks last-status)
                 worktree)))
```

### 9.2 Conflict Scenarios

| Scenario | Detection | Resolution | Prevention |
|----------|-----------|-----------|-----------|
| **Overlapping File Edits** | Scan modified files across worktrees | Suggest merge order; manual resolution if needed | Tag tasks more granularly |
| **Dependency Version Conflicts** | Parse package.json/requirements.txt | Prompt: "Align dependency versions? (y/n)" | Run `npm install` in shared base first |
| **API Contract Changes** | LLM analyzes code changes | Alert frontend agent; suggest refactor | Define API contract upfront in PRD |
| **Database Migration Conflicts** | Multiple agents create migrations | Merge migrations manually; reorder timestamps | Serialize DB tasks (no parallel) |
| **Git Merge Conflicts** | Merge attempt fails | Drop to merge tool; pause agents | Better dependency tagging |

**Conflict Detection Implementation:**
```lisp
(defun detect-file-overlaps (worktrees)
  "Find files modified in multiple worktrees"
  (let ((file-map (make-hash-table :test 'equal)))
    (dolist (wt worktrees)
      (dolist (file (git-modified-files wt))
        (push wt (gethash file file-map))))
    ; Return files with >1 worktree
    (loop for file being the hash-keys of file-map
          when (> (length (gethash file file-map)) 1)
          collect (list file (gethash file file-map)))))
```

### 9.3 User Errors

| Error | Cause | System Response | Fix |
|-------|-------|----------------|-----|
| **Invalid PRD** | Malformed or empty PRD file | Show error; request valid file | Provide PRD template |
| **No Anthropic API Key** | Missing `ANTHROPIC_API_KEY` | Prompt for key; save to config | Guide to obtain key |
| **Git Not Clean** | Uncommitted changes in main | Error: "Commit or stash changes first" | User commits/stashes |
| **Taskmaster Not Found** | CLI not installed | Install instructions shown | `npm install -g task-master-ai` |
| **Resource Exhaustion** | Too many parallel agents for system | Limit agents to 4; queue extras | Upgrade hardware or reduce parallel agents |
| **Wrong Directory** | Not in project root | Error: "No .git found. Run from repo root." | `cd` to correct location |

**Error Handling Example:**
```lisp
(defun validate-environment ()
  "Check prerequisites before starting"
  (unless (probe-file ".git")
    (error "Not a git repository. Run from project root."))
  
  (unless (uiop:getenv "ANTHROPIC_API_KEY")
    (format t "⚠️ Anthropic API key not found.~%")
    (format t "Get key from: https://console.anthropic.com~%")
    (format t "Set with: export ANTHROPIC_API_KEY=your_key~%")
    (error "Missing API key"))
  
  (unless (zerop (nth-value 2 (uiop:run-program "zellij --version" :ignore-error-status t)))
    (error "Zellij not found. Install with: cargo install zellij"))
  
  (unless (zerop (nth-value 2 (uiop:run-program "task-master --version" :ignore-error-status t)))
    (error "Taskmaster not found. Install with: npm install -g task-master-ai")))
```

### 9.4 Data Loss Prevention

| Risk | Mitigation | Verification |
|------|-----------|--------------|
| **Uncommitted Work Lost** | Auto-commit every subtask completion | Check git log |
| **Session State Lost** | Auto-save to `.paos/session-{id}.lisp` every 60s | Resume test after crash |
| **Status File Corruption** | Write to temp file; atomic rename | JSON validation on read |
| **Worktree Premature Deletion** | Confirmation prompt before cleanup | Manual delete only after merge |
| **Config Overwrite** | Backup to `.paos/config.backup` before edit | Diff check |

**Auto-Save Implementation:**
```lisp
(defun auto-save-loop ()
  "Periodically save session state"
  (bordeaux-threads:make-thread
    (lambda ()
      (loop
        (sleep 60)
        (handler-case
            (save-session-to-disk *current-session*)
          (error (e)
            (log-error "Failed to auto-save session: ~A" e)))))))

(defun save-session-to-disk (session)
  "Atomically save session state"
  (let ((temp-file (format nil "~A.tmp" (session-file-path session)))
        (final-file (session-file-path session)))
    (with-open-file (stream temp-file :direction :output :if-exists :supersede)
      (write session :stream stream))
    (rename-file temp-file final-file)))
```

---

## 10. Future Enhancements

### Phase 2: Enhanced UX (3-6 months post-v1)

**P2.1: Web Dashboard (Optional)**
- **Goal:** Provide browser-based alternative to terminal UI
- **Tech:** Phoenix LiveView (if Elixir) or similar real-time framework
- **Features:**
  - Live-updating agent cards
  - Code diff viewer
  - One-click PR review
  - Mobile-responsive for remote monitoring
- **Effort:** 3-4 weeks

**P2.2: Agent Templates**
- **Goal:** Reusable task patterns for common scenarios
- **Examples:**
  - "Add REST endpoint" template
  - "Create React component" template
  - "Database migration" template
- **Implementation:** YAML templates + Claude expansion
- **Effort:** 1-2 weeks

**P2.3: Cost Optimization**
- **Goal:** Reduce API costs without sacrificing quality
- **Strategy:**
  - Use Claude Haiku for simple tasks, Sonnet for complex
  - Cache frequently-used prompts
  - Batch status analyses
- **Effort:** 1 week

**P2.4: Learning from History**
- **Goal:** System learns from successful executions
- **Features:**
  - Save successful task trees as templates
  - Suggest similar past solutions
  - Auto-tag based on historical patterns
- **Storage:** SQLite database of sessions
- **Effort:** 2-3 weeks

### Phase 3: Enterprise Features (6-12 months)

**P3.1: Multi-Repository Orchestration**
- **Goal:** Coordinate agents across microservices
- **Challenge:** Cross-repo dependencies and PRs
- **Solution:** 
  - Meta-orchestrator managing multiple PAOS instances
  - Shared status directory
  - Coordinated merge order
- **Effort:** 4-6 weeks

**P3.2: Team Collaboration**
- **Goal:** Multiple developers sharing orchestrator
- **Features:**
  - User authentication
  - Agent assignment (Alice gets frontend, Bob gets backend)
  - Shared dashboard with per-user focus
- **Effort:** 6-8 weeks

**P3.3: CI/CD Integration**
- **Goal:** Run PAOS in automated pipelines
- **Trigger:** On PR creation, run PAOS to implement feature
- **Implementation:**
  - GitHub Actions integration
  - Headless mode (no Zellij)
  - Status via webhooks
- **Effort:** 3-4 weeks

**P3.4: Advanced Conflict Resolution**
- **Goal:** LLM-powered merge suggestions
- **Process:**
  - Detect conflicts
  - Use Claude to analyze both sides
  - Generate merge resolution
  - Human review before apply
- **Effort:** 2-3 weeks

### Phase 4: Experimental (12+ months)

**P4.1: Self-Healing Agents**
- **Goal:** Agents detect and fix their own errors without human input
- **Mechanism:**
  - Test failure → analyze error → propose fix → retry
  - Max 3 self-healing attempts before escalation
- **Safety:** Extensive sandbox testing required
- **Effort:** 6-8 weeks

**P4.2: Code Review Agent**
- **Goal:** Dedicated agent critiques PRs
- **Capabilities:**
  - Security analysis
  - Performance suggestions
  - Style consistency
- **Integration:** Posts GitHub PR comments
- **Effort:** 4-6 weeks

**P4.3: Documentation Agent**
- **Goal:** Auto-generate docs from execution traces
- **Output:**
  - API documentation
  - Architecture diagrams
  - User guides
- **Tech:** Mermaid for diagrams, Markdown for docs
- **Effort:** 3-4 weeks

**P4.4: Voice Interface**
- **Goal:** Control PAOS via speech
- **Commands:** "Pause backend agent", "Show me conflicts"
- **Tech:** Whisper for transcription
- **Effort:** 2-3 weeks

### Non-Feature Improvements

**Performance:**
- Parallel status polling (current: sequential)
- Status file caching with inotify watches
- Compiled Lisp binary for faster startup

**Developer Experience:**
- VSCode extension for PAOS integration
- Emacs major mode for orchestrator REPL
- Better error messages with actionable suggestions

**Observability:**
- Prometheus metrics export
- Agent performance analytics
- Cost tracking dashboard

---

## 11. Implementation Plan

### Phase 1: MVP (2-3 weeks)

**Week 1: Core Orchestrator**

**Days 1-2: Project Setup**
- [x] Create ASDF system definition
- [x] Setup Quicklisp dependencies
- [x] Implement config file loading
- [x] Basic logging infrastructure

**Days 3-5: Task Decomposition**
- [x] Taskmaster CLI integration (`decompose-prd`)
- [x] Tag assignment via Claude API
- [x] User review loop (terminal input)
- [x] YAML parsing and validation

**Days 6-7: Agent Spawning**
- [x] Git worktree creation
- [x] Project environment detection and init
- [x] Zellij integration (tab creation)
- [x] Agent context file generation

**Week 2: Monitoring & Dashboard**

**Days 8-10: Status Protocol**
- [x] Status file schema design
- [x] Polling loop implementation
- [x] Agent health checking

**Days 11-12: Dashboard UI**
- [x] ANSI terminal rendering
- [x] Progress bars and coloring
- [x] Log tailing display

**Days 13-14: Orchestrator Intelligence**
- [x] Claude API integration for analysis
- [x] Conflict detection logic
- [x] Recommendation engine

**Week 3: Integration & Polish**

**Days 15-16: PR Workflow**
- [x] GitHub CLI integration
- [x] PR creation with rich templates
- [x] Review assistance

**Days 17-18: Merge Coordination**
- [x] Dependency-aware merge order
- [x] Post-merge validation
- [x] Worktree cleanup

**Days 19-20: Testing & Documentation**
- [x] End-to-end test with sample PRD
- [x] Write README and user guide
- [x] Create tutorial video

**Day 21: Release**
- [x] Tag v1.0.0
- [x] Publish to GitHub
- [x] Announce to beta users

### Phase 2: Polish & Scaling (1-2 weeks)

**Week 4: Enhanced Features**

**Days 22-23: Conflict Detection++**
- [ ] Dependency version analysis
- [ ] API contract change detection
- [ ] Smarter merge suggestions

**Days 24-25: Error Recovery**
- [ ] Agent restart mechanism
- [ ] Session recovery from crashes
- [ ] Retry logic with backoff

**Days 26-27: Dashboard Improvements**
- [ ] Color themes
- [ ] Keyboard shortcuts
- [ ] Status filters (show only blocked, etc.)

**Day 28: Performance Tuning**
- [ ] Profile orchestrator loop
- [ ] Optimize status file I/O
- [ ] Reduce API call frequency

**Week 5: Scaling & Docs**

**Days 29-30: Multi-Agent Scaling**
- [ ] Support 8 parallel agents
- [ ] Resource monitoring (CPU/RAM)
- [ ] Agent queuing system

**Days 31-32: Comprehensive Logging**
- [ ] Structured logs (JSON)
- [ ] Log rotation
- [ ] Debug mode

**Days 33-35: Documentation**
- [ ] API reference for Lisp modules
- [ ] Troubleshooting guide
- [ ] Example PRD library

### Milestones

| Milestone | Date | Deliverables |
|-----------|------|--------------|
| **M1: Core Working** | End of Week 1 | Can spawn agents and monitor basic status |
| **M2: Full Workflow** | End of Week 2 | End-to-end: PRD → agents → PRs → merge |
| **M3: MVP Release** | End of Week 3 | v1.0.0 tagged, usable by beta testers |
| **M4: Production Ready** | End of Week 5 | Stable, documented, tested |

### Testing Strategy

**Unit Tests (Ongoing):**
```lisp
;; tests/test-decomposer.lisp
(define-test test-taskmaster-integration
  "Verify Taskmaster CLI parsing works"
  (let ((tasks (decompose-prd "examples/simple-prd.txt")))
    (assert-true (> (length (tasks-list tasks)) 0))
    (assert-true (every #'task-has-description-p (tasks-list tasks)))))

;; tests/test-spawner.lisp
(define-test test-worktree-creation
  "Verify Git worktree created correctly"
  (let ((worktree (create-worktree 'test "/tmp/test-project")))
    (assert-true (probe-file worktree))
    (assert-true (git-branch-exists-p worktree "test/*"))
    (cleanup-worktree worktree)))
```

**Integration Tests:**
```bash
#!/bin/bash
# tests/integration-test.sh

# Test: Full workflow with sample PRD
cd examples/oauth-feature
../../bin/paos-test start prd.txt --auto-approve --no-interactive

# Verify outputs
[ -f ".paos/session-*.log" ] || exit 1
[ $(ls ../worktree-* | wc -l) -eq 3 ] || exit 1

# Verify PRs created
gh pr list | grep -q "\[backend\]" || exit 1
gh pr list | grep -q "\[frontend\]" || exit 1

echo "✓ Integration test passed"
```

**Dogfooding:**
- Use PAOS to build PAOS features (Phase 2+)
- Track issues in `.paos/dogfood.log`
- Iterate based on real usage

**Beta Testing:**
- Recruit 3-5 developers from different domains
- Provide simple PRDs (auth, CRUD, etc.)
- Weekly feedback sessions
- Iterate on UX pain points

---

## 12. Open Questions & Decisions

### Critical Decisions (Need Resolution Before MVP)

**Q1: Lisp vs. Elixir — Final Choice?**
- **Recommendation:** Common Lisp (SBCL)
- **Trade-off:** Better REPL/control vs. better concurrency primitives
- **Decision Maker:** Lead developer
- **Deadline:** Before Day 1

**Q2: Zellij vs. Tmux — Confirmed?**
- **Recommendation:** Zellij (better UX, simpler API)
- **Fallback:** Tmux if Zellij unavailable on target system
- **Decision:** Use Zellij, detect and warn if missing
- **Status:** ✅ Confirmed

**Q3: Status Protocol — JSON Files or Message Queue?**
- **Options:**
  - **A)** JSON files (simple, debuggable)
  - **B)** Redis pub/sub (faster, scalable)
- **Recommendation:** JSON files for v1; Redis in Phase 3
- **Reasoning:** Simplicity > performance for MVP
- **Status:** ✅ Confirmed JSON

**Q4: Checkpoint Frequency — How Often Should Agents Pause?**
- **Options:**
  - Every commit (high safety, slow)
  - Every subtask (balanced)
  - Only on blockers (fast, risky)
- **Recommendation:** Every subtask by default; configurable per project
- **Config:** `.paos/checkpoints.yaml`
- **Status:** ✅ Decided — every subtask

### Non-Critical (Can Defer)

**Q5: Cost Management — Hard Limits on API Calls?**
- **Issue:** Runaway costs if agents loop indefinitely
- **Options:**
  - Hard cap: $50/session
  - Warning at $25
  - No limit (trust developer)
- **Decision:** Defer to Phase 2; add warning at $25 first

**Q6: Product Name — Keep "PAOS" or Rebrand?**
- **Current:** PAOS (Parallel Agent Orchestration System)
- **Alternatives:** 
  - "Swarm" (simple, evocative)
  - "Conductor" (musical metaphor)
  - "Foreman" (construction metaphor)
- **Decision:** Keep PAOS for technical docs; consider branding later

**Q7: License — Open Source or Proprietary?**
- **Options:**
  - MIT (permissive)
  - AGPL (copyleft)
  - Proprietary with free tier
- **Recommendation:** MIT for community adoption
- **Decision Maker:** Project sponsor

**Q8: Hosting — Self-Hosted Only or Offer Cloud?**
- **v1:** Self-hosted only
- **Future:** Could offer cloud-hosted orchestrator (SaaS model)
- **Decision:** Self-hosted for now; revisit in Phase 3

### Research Questions

**R1: Can Agents Self-Coordinate Without Orchestrator?**
- **Hypothesis:** Agents communicate directly via shared files
- **Experiment:** Phase 4 exploration
- **Risk:** Complexity explosion

**R2: What's Optimal Number of Parallel Agents?**
- **Hypothesis:** 4-6 agents on typical hardware
- **Experiment:** Stress test with 2, 4, 6, 8, 10 agents
- **Metric:** Time-to-completion vs. resource usage

**R3: Can We Predict Task Duration?**
- **Goal:** Show "ETA: 2h 30m" in dashboard
- **Approach:** Historical data + LLM estimation
- **Accuracy Target:** ±30 minutes

---

## 13. Appendices

### Appendix A: Example PRD Templates

#### Template 1: Full-Stack Feature

```markdown
# Feature: {Feature Name}

## Overview
{1-2 sentence description}

## Requirements
### Functional
- {Requirement 1}
- {Requirement 2}
- {Requirement 3}

### Non-Functional
- Performance: {criteria}
- Security: {criteria}
- Accessibility: {criteria}

## Technical Constraints
- Backend: {technology}
- Frontend: {technology}
- Database: {technology}
- APIs: {external services}

## User Stories
As a {user type}, I want to {action} so that {benefit}.

## Success Criteria
- {Measurable criterion 1}
- {Measurable criterion 2}
- {Measurable criterion 3}

## Out of Scope (v1)
- {Deferred feature 1}
- {Deferred feature 2}
```

#### Template 2: Bug Fix

```markdown
# Bug Fix: {Issue Description}

## Problem
{Detailed description of bug}

## Current Behavior
{What happens now}

## Expected Behavior
{What should happen}

## Reproduction Steps
1. {Step 1}
2. {Step 2}
3. {Step 3}

## Affected Components
- {Component 1}
- {Component 2}

## Root Cause Hypothesis
{Best guess at underlying issue}

## Proposed Solution
{High-level fix approach}

## Testing Requirements
- {Test 1}
- {Test 2}
```

#### Template 3: Refactoring

```markdown
# Refactoring: {Goal}

## Motivation
{Why this refactor is needed}

## Current Architecture
{Brief description of current state}

## Target Architecture
{Desired end state}

## Migration Strategy
{How to transition}

## Risk Assessment
- {Risk 1 and mitigation}
- {Risk 2 and mitigation}

## Success Criteria
- {Criterion 1}
- {Criterion 2}
- No regressions in test suite

## Timeline Constraints
{Any deadlines or dependencies}
```

### Appendix B: Sample Task Breakdown

**PRD:** "Add OAuth2 authentication with Google and GitHub"

**Taskmaster Output:**
```yaml
session_id: oauth-auth-1729098721
created_at: 2025-10-16T14:00:00Z
prd_file: prd.txt

high_level_tasks:
  - id: 1
    description: "Implement OAuth backend routes and Passport strategies"
    tags: [backend, auth]
    dependencies: []
    effort: high
    estimated_hours: 8
    
    subtasks:
      - id: 1.1
        description: "Install and configure Passport.js"
        files: [package.json, config/passport.js]
        
      - id: 1.2
        description: "Implement Google OAuth strategy"
        files: [strategies/google.js, .env.example]
        
      - id: 1.3
        description: "Implement GitHub OAuth strategy"
        files: [strategies/github.js]
        
      - id: 1.4
        description: "Create /auth routes and callbacks"
        files: [routes/auth.js, app.js]
        
      - id: 1.5
        description: "Setup session management"
        files: [middleware/session.js]
        
  - id: 2
    description: "Design and build OAuth UI components"
    tags: [frontend, ui]
    dependencies: []
    effort: medium
    estimated_hours: 6
    
    subtasks:
      - id: 2.1
        description: "Create OAuthButton component"
        files: [components/OAuthButton.tsx, components/OAuthButton.test.tsx]
        
      - id: 2.2
        description: "Build provider selection screen"
        files: [pages/Login.tsx, styles/Login.css]
        
      - id: 2.3
        description: "Implement loading and error states"
        files: [components/LoadingSpinner.tsx, components/ErrorMessage.tsx]
        
      - id: 2.4
        description: "Add provider icons and styling"
        files: [assets/icons/*, styles/oauth.css]
        
  - id: 3
    description: "Database schema for OAuth providers"
    tags: [db, backend]
    dependencies: []
    effort: low
    estimated_hours: 3
    
    subtasks:
      - id: 3.1
        description: "Create oauth_providers table migration"
        files: [migrations/20251016_create_oauth_providers.sql]
        
      - id: 3.2
        description: "Update user model with provider associations"
        files: [models/User.js, migrations/20251016_add_user_provider_fk.sql]
        
      - id: 3.3
        description: "Add indexes for performance"
        files: [migrations/20251016_add_oauth_indexes.sql]
        
  - id: 4
    description: "E2E and unit tests for OAuth flows"
    tags: [tests]
    dependencies: [1, 2, 3]
    effort: medium
    estimated_hours: 6
    
    subtasks:
      - id: 4.1
        description: "Unit tests for Passport strategies"
        files: [test/strategies/*.test.js]
        
      - id: 4.2
        description: "E2E test for Google login"
        files: [test/e2e/google-oauth.test.js]
        
      - id: 4.3
        description: "E2E test for GitHub login"
        files: [test/e2e/github-oauth.test.js]
        
      - id: 4.4
        description: "Test account linking scenarios"
        files: [test/integration/account-linking.test.js]
        
      - id: 4.5
        description: "Test error handling and edge cases"
        files: [test/integration/oauth-errors.test.js]

parallel_groups:
  - name: backend
    tasks: [1]
    can_run_parallel: true
    
  - name: frontend
    tasks: [2]
    can_run_parallel: true
    
  - name: db
    tasks: [3]
    can_run_parallel: true
    
  - name: tests
    tasks: [4]
    can_run_parallel: false
    wait_for: [1, 2, 3]

estimated_total_hours: 23
estimated_parallel_hours: 8 (with 3 parallel agents)
efficiency_gain: 2.9x
```

### Appendix C: Common Lisp Code Skeleton

**File Structure:**
```
paos/
├── paos.asd                      # ASDF system definition
├── paos-init.lisp                # Entry point for SBCL
├── core/
│   ├── package.lisp              # Package definitions
│   ├── config.lisp               # Configuration loading
│   ├── orchestrator.lisp         # Main coordinator
│   ├── decomposer.lisp           # Task breakdown
│   ├── spawner.lisp              # Agent/worktree management
│   └── monitor.lisp              # Status polling
├── agents/
│   ├── prompts.lisp              # Prompt templates
│   └── tools.lisp                # Tool definitions
├── integrations/
│   ├── taskmaster.lisp           # Taskmaster CLI wrapper
│   ├── zellij.lisp               # Zellij session management
│   ├── github.lisp               # GitHub API
│   └── anthropic.lisp            # Claude API client
├── ui/
│   └── dashboard.lisp            # Terminal UI
├── utils/
│   ├── git.lisp                  # Git operations
│   ├── file-io.lisp              # Status file I/O
│   └── logging.lisp              # Logging utilities
└── tests/
    ├── test-decomposer.lisp
    ├── test-spawner.lisp
    └── test-monitor.lisp
```

**Core Modules:**

**paos.asd:**
```lisp
(defsystem "paos"
  :version "1.0.0"
  :author "Your Name"
  :license "MIT"
  :depends-on ("cl-json"
               "dexador"
               "bordeaux-threads"
               "uiop"
               "cl-yaml"
               "cl-ppcre"
               "local-time"
               "alexandria"
               "str")
  :components ((:module "core"
                :components ((:file "package")
                             (:file "config" :depends-on ("package"))
                             (:file "orchestrator" :depends-on ("config"))
                             (:file "decomposer" :depends-on ("config"))
                             (:file "spawner" :depends-on ("config"))
                             (:file "monitor" :depends-on ("config"))))
               (:module "agents"
                :components ((:file "prompts")
                             (:file "tools")))
               (:module "integrations"
                :components ((:file "taskmaster")
                             (:file "zellij")
                             (:file "github")
                             (:file "anthropic")))
               (:module "ui"
                :components ((:file "dashboard")))
               (:module "utils"
                :components ((:file "git")
                             (:file "file-io")
                             (:file "logging"))))
  :in-order-to ((test-op (test-op "paos/tests"))))

(defsystem "paos/tests"
  :depends-on ("paos" "fiveam")
  :components ((:module "tests"
                :components ((:file "test-decomposer")
                             (:file "test-spawner")
                             (:file "test-monitor"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :paos-tests)))
```

**core/package.lisp:**
```lisp
(defpackage :paos
  (:use :cl)
  (:export 
   ;; Main API
   #:start
   #:approve-tasks
   #:status
   #:focus
   #:pause
   #:resume
   #:review
   #:merge
   #:quit
   
   ;; Session management
   #:save-session
   #:resume-session
   
   ;; Configuration
   #:load-config
   #:configure-api-key))

(in-package :paos)
```

**core/orchestrator.lisp:**
```lisp
(in-package :paos)

(defvar *current-session* nil
  "Currently active PAOS session")

(defclass session ()
  ((id :initarg :id :accessor session-id)
   (zellij-name :initarg :zellij-name :accessor session-zellij-name)
   (tasks :initarg :tasks :accessor session-tasks)
   (agents :initform nil :accessor session-agents)
   (start-time :initform (get-universal-time) :accessor session-start-time)))

(defun start (prd-file)
  "Entry point: parse PRD and begin orchestration"
  (validate-environment)
  (load-config)
  
  (let* ((session-id (get-universal-time))
         (zellij-name (format nil "paos-~A" session-id)))
    
    (format t "Initializing PAOS session...~%")
    
    ;; Create Zellij session
    (zellij:create-session zellij-name)
    
    ;; Decompose PRD
    (format t "✓ Parsing PRD with Taskmaster...~%")
    (let ((tasks (decomposer:decompose-prd prd-file)))
      
      ;; Tag for parallelism
      (format t "✓ Assigning parallelism tags...~%")
      (setf tasks (decomposer:assign-parallel-tags tasks prd-file))
      
      ;; Display for review
      (display-task-summary tasks)
      
      ;; Create session
      (setf *current-session* 
            (make-instance 'session
                          :id session-id
                          :zellij-name zellij-name
                          :tasks tasks))
      
      (format t "~%Review tasks? (y/n/edit) ")
      (let ((response (read-line)))
        (cond
          ((string= response "y") (approve-tasks))
          ((string= response "edit") (edit-tasks-interactive))
          (t (format t "Aborting.~%")))))))

(defun approve-tasks ()
  "User approves task breakdown; spawns agents"
  (let ((session *current-session*))
    ;; Expand subtasks
    (format t "~%Expanding subtasks...~%")
    (dolist (task (session-tasks session))
      (setf (task-subtasks task) 
            (decomposer:expand-subtasks task))
      (format t "✓ Task ~A expanded (~A subtasks)~%" 
              (task-id task) 
              (length (task-subtasks task))))
    
    ;; Group by tags
    (let ((groups (group-tasks-by-tag (session-tasks session))))
      
      (format t "~%Approve and spawn agents? (yes/no) ")
      (when (string= (read-line) "yes")
        
        ;; Spawn agents for each group
        (dolist (group groups)
          (let ((tag (car group))
                (tasks (cdr group)))
            (spawn-agent-for-group tag tasks)))
        
        ;; Enter monitoring loop
        (monitoring-loop (mapcar #'car groups))))))

(defun monitoring-loop (agent-tags &optional (max-iterations 10000))
  "Main monitoring loop: poll statuses, update dashboard"
  (format t "~%Entering monitoring mode...~%~%")
  (start-auto-save)
  
  (loop repeat max-iterations
        for statuses = (monitor:poll-agent-statuses agent-tags)
        do (progn
             (ui:display-dashboard statuses)
             
             ;; Orchestrator analysis every 30s
             (when (zerop (mod (get-universal-time) 30))
               (let ((feedback (monitor:orchestrator-analyze statuses)))
                 (when feedback
                   (ui:display-orchestrator-feedback feedback))))
             
             ;; Check for completion
             (when (all-agents-complete-p statuses)
               (format t "~%All agents complete! 🎉~%")
               (return))
             
             ;; Check for input
             (when (listen *standard-input*)
               (handle-repl-input))
             
             (sleep 5))))

(defun quit ()
  "Gracefully shutdown all agents and exit"
  (when *current-session*
    (save-session-to-disk *current-session*)
    (format t "Session saved. Goodbye!~%"))
  (uiop:quit))
```

**Remaining modules follow similar patterns...**

### Appendix D: Zellij Configuration

**Zellij Layout for PAOS:**

**File:** `~/.config/zellij/paos-layout.kdl`

```kdl
layout {
    default_tab_template {
        pane size=1 borderless=true {
            plugin location="zellij:tab-bar"
        }
        children
        pane size=2 borderless=true {
            plugin location="zellij:status-bar"
        }
    }

    tab name="Dashboard" focus=true {
        pane split_direction="vertical" {
            pane size="80%" {
                // Dashboard auto-refresh display
                command "watch"
                args "-n" "2" "cat" "/tmp/paos-dashboard.txt"
            }
            pane size="20%" {
                // REPL
                command "sbcl"
                args "--load" "paos-init.lisp"
            }
        }
    }

    // Agent tabs created dynamically by spawner
    // No need to pre-define them
}
```

**Status Bar Configuration:**

**File:** `~/.config/zellij/config.kdl`

```kdl
theme "paos-theme"

themes {
    paos-theme {
        fg "#D8DEE9"
        bg "#2E3440"
        black "#3B4252"
        red "#BF616A"
        green "#A3BE8C"
        yellow "#EBCB8B"
        blue "#81A1C1"
        magenta "#B48EAD"
        cyan "#88C0D0"
        white "#E5E9F0"
        orange "#D08770"
    }
}

keybinds {
    normal {
        bind "Ctrl t" { SwitchToMode "Tab"; }
        bind "Ctrl p" { SwitchToMode "Pane"; }
        bind "Ctrl o" { SwitchToMode "Session"; }
    }
}
```

### Appendix E: Troubleshooting Guide

#### Issue: Agent Not Responding

**Symptoms:**
- Status file not updating for >2 minutes
- Dashboard shows "⚠️ Agent unresponsive"

**Diagnosis:**
```lisp
* (status 'backend)
; Shows last update timestamp

* (focus 'backend)
; Switch to agent tab to see terminal
```

**Solutions:**
1. Agent crashed → Restart: `(restart-agent 'backend)`
2. Long-running task → Wait or provide input
3. Network issue → Check API key, connection

#### Issue: Git Worktree Creation Fails

**Symptoms:**
- Error: "fatal: 'worktree-ui' already exists"

**Diagnosis:**
```bash
git worktree list
# Check if worktree exists
```

**Solutions:**
1. Remove existing: `git worktree remove ../worktree-ui`
2. Use different name: `(create-worktree 'ui-v2 "..")`
3. Check disk space: `df -h`

#### Issue: Taskmaster Parse Failure

**Symptoms:**
- Error: "Taskmaster returned empty result"
- PRD not broken down

**Diagnosis:**
```bash
task-master parse-prd prd.txt
# Run manually to see error
```

**Solutions:**
1. Check PRD format (must be valid Markdown/text)
2. Verify API key: `echo $ANTHROPIC_API_KEY`
3. Network connection: `curl https://api.anthropic.com`

#### Issue: High API Costs

**Symptoms:**
- API usage warnings
- Unexpected charges

**Diagnosis:**
```lisp
* (show-api-usage)
; Display session API call count and cost
```

**Solutions:**
1. Reduce orchestrator analysis frequency (60s → 120s)
2. Use smaller models: Haiku instead of Sonnet for simple tasks
3. Set hard limit: `(set-api-budget 50)` for $50 max

---

## Document Control

**Version History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 0.1 | 2025-10-16 | Initial Draft | First complete PRD |
| 1.0 | 2025-10-16 | Final Review | Added Zellij integration, refined architecture |

**Approvals:**

- [ ] Product Lead: _______________
- [ ] Engineering Lead: _______________
- [ ] Design Lead: _______________

**Next Review Date:** 2025-11-16 (1 month post-release)

---

**End of Document**
