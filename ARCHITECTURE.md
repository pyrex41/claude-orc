# PAOS Architecture Documentation

## System Overview

PAOS (Parallel Agent Orchestration System) is a sophisticated Common Lisp application for orchestrating multiple Claude Code agents in parallel development workflows.

## Module Organization

### Core Layer (`core/`)
Foundation modules with minimal dependencies:

- **`package.lisp`** - All package definitions and exports
- **`config.lisp`** - YAML configuration + environment variables
- **`decomposer.lisp`** - Taskmaster CLI integration
- **`git.lisp`** - Git worktree management

### Extended Features (`src/`)
Enhanced functionality built on core:

#### Parsing & Analysis
- **`prd-parser.lisp`** - Multi-format PRD parsing (MD, YAML, TXT)
- **`ai-integration.lisp`** - Claude API for PRD analysis

#### Planning & Organization
- **`tagger.lisp`** - Domain-specific task tagging
- **`expander.lisp`** - Hierarchical subtask expansion

#### Orchestration
- **`zellij-integration.lisp`** - Terminal multiplexer control
- **`agent-communication.lisp`** - Direct agent commands
- **`aci-tools.lisp`** - Agent tool definitions (ACI)

#### Monitoring & Intelligence
- **`dashboard.lisp`** - ANSI terminal UI
- **`status-protocol.lisp`** - JSON status communication
- **`orchestrator-intelligence.lisp`** - LLM-based analysis
- **`conflict-detection.lisp`** - Proactive conflict detection

#### User Interface
- **`repl.lisp`** - Interactive command interface
- **`main-interface.lisp`** - Unified orchestrator interface
- **`checkpoints.lisp`** - Pause/resume system

#### Infrastructure
- **`reliability.lisp`** - State persistence & recovery
- **`security.lisp`** - Encryption, auditing, sandboxing
- **`performance.lisp`** - Caching & optimization
- **`usability.lisp`** - Help system & error messages

### Integrations (`integrations/`)
External service integrations:

- **`github.lisp`** - GitHub CLI for PR management
- **`review.lisp`** - AI-powered PR review
- **`merge.lisp`** - Dependency-aware merging

## Data Flow

```
PRD File
  ↓
[PRD Parser] → Tasks (JSON)
  ↓
[Tagger] → Tagged Tasks
  ↓
[Expander] → Hierarchical Tasks
  ↓
[Worktree Manager] → Isolated Environments
  ↓
[Agent Spawner] → Claude Code Instances
  ↓
[Status Protocol] → JSON Status Files
  ↓
[Monitor] → Dashboard Display
  ↑
[Orchestrator Intelligence] → Recommendations
  ↓
[Conflict Detector] → Alerts
  ↓
[PR Manager] → Pull Requests
  ↓
[Merge Coordinator] → Integrated Code
```

## Key Design Patterns

### External Process Integration
All external tools (git, gh, zellij, task-master) via `uiop:run-program`

### JSON Communication
Status files, API calls, and configuration use JSON for interoperability

### CLOS for State
Object-oriented design for complex state (dashboard, checkpoints, agents)

### Background Threads
Non-blocking operations with bordeaux-threads (dashboard, monitoring, auto-save)

### Callback Pattern
Services accept callback functions for event handling

### Hash Table Data
Flexible key-value structures for tasks, configuration, caches

## Security Model

1. **Encryption**: AES-256 for API keys
2. **Auditing**: JSON audit logs for all operations
3. **Sanitization**: Regex-based secret redaction
4. **Sandboxing**: Command/path whitelisting
5. **Validation**: Input validation at all boundaries

## Performance Considerations

- **Caching**: 2-60s TTL for expensive operations
- **Batching**: Group operations to reduce overhead
- **Threading**: Background updates don't block UI
- **Differential Rendering**: Dashboard updates only changed lines
- **Lazy Loading**: Load data only when needed

## Extension Points

### Adding New Tools
```lisp
(paos/aci:define-tool "tool_name" "Description" 
                     parameters handler-fn
                     :category "category")
```

### Adding REPL Commands
Extend `process-command` in `repl.lisp`

### Adding Help Topics
```lisp
(paos/usability:register-help-topic name desc content
                                   :examples '(...)
                                   :see-also '(...))
```

### Adding Monitoring Services
Follow pattern in `main-interface.lisp` startup sequence
