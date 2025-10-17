# System Patterns

## Architecture Overview

### Core Components
```
PAOS System
├── Configuration Layer (core/config.lisp)
│   ├── YAML config loading
│   ├── Environment variable integration
│   └── API key management
│
├── Task Decomposition (core/decomposer.lisp)
│   ├── Taskmaster CLI integration
│   ├── PRD parsing (planned expansion)
│   └── YAML output processing
│
├── Git Management (core/git.lisp)
│   ├── Worktree creation
│   ├── Branch management
│   └── Path formatting
│
├── Agent Orchestration (planned)
│   ├── Zellij integration
│   ├── Claude Code spawning
│   └── Status monitoring
│
└── User Interface (planned)
    ├── Real-time dashboard
    ├── REPL commands
    └── Direct agent chat
```

## Key Technical Decisions

### 1. Common Lisp + SBCL
- **Why**: Excellent REPL for interactive development, strong threading support, mature package ecosystem
- **Trade-offs**: Smaller community than Python/Node, but better suited for complex orchestration logic

### 2. Git Worktrees for Isolation
- **Why**: Native Git feature, no container overhead, easy to manage
- **Pattern**: Each agent group gets isolated worktree with feature branch
- **Naming**: `.paos/worktrees/{tag}-{timestamp}`

### 3. Taskmaster CLI Integration
- **Why**: Proven task decomposition, AI-powered, well-maintained
- **Pattern**: External process call via `uiop:run-program`, parse YAML output with cl-yaml
- **Error Handling**: Graceful fallback on CLI failures

### 4. Zellij for Terminal Multiplexing
- **Why**: Modern tmux alternative, better API, tab support
- **Pattern**: One tab per agent group, orchestrator in main tab
- **Future**: Could expand to tmux support

### 5. Configuration Management
- **Pattern**: YAML files + environment variables
- **Precedence**: Env vars override YAML values
- **Security**: API keys never in version control, encrypted at rest (planned)

## Design Patterns in Use

### External Process Integration
```lisp
(defun run-external-command (cmd args)
  "Pattern for calling external tools safely"
  (handler-case
      (uiop:run-program (cons cmd args) 
                        :output :string 
                        :error-output :string)
    (error (e) 
      (handle-process-error e))))
```

### Configuration Loading
```lisp
(defun load-config ()
  "Pattern: YAML + env vars with validation"
  (let ((config (parse-yaml-file ".paos/config.yaml")))
    (merge-env-vars config)
    (validate-required-fields config)
    config))
```

### Worktree Management
```lisp
(defun create-worktree (tag)
  "Pattern: Isolated environments with cleanup"
  (let ((path (format-worktree-path tag)))
    (ensure-worktree-root-exists)
    (run-git-worktree-add path tag)
    (initialize-worktree-env path)))
```

## Component Relationships

### Dependency Flow
1. **Config** → Everything (provides API keys, settings)
2. **Decomposer** → Uses Config for API keys
3. **Git** → Independent, provides isolation
4. **Spawner** (planned) → Uses Git + Config
5. **Dashboard** (planned) → Uses Monitor + Config
6. **Orchestrator** (planned) → Coordinates all components

### Data Flow
1. PRD File → Decomposer → Task JSON
2. Task JSON → Tagger → Tagged Tasks
3. Tagged Tasks → Expander → Subtasks
4. Tasks → Spawner → Agents in Worktrees
5. Agents → Status Files → Monitor → Dashboard
6. Monitor → Orchestrator → LLM Analysis → Recommendations

### State Management
- **Persisted State**: `.paos/state.json` for session recovery (planned)
- **Runtime State**: In-memory hash tables for active agents
- **Shared State**: JSON files for agent-orchestrator communication
