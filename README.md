# PAOS - Parallel Agent Orchestration System

A Common Lisp system for orchestrating multiple Claude Code agents to work on task decomposition and development workflows in parallel.

## Overview

PAOS (Parallel Agent Orchestration System) is a terminal-based multi-agent orchestration platform that:

1. Parses PRDs into structured tasks using AI
2. Spawns Claude Code instances in isolated Git worktrees
3. Manages agents through a Zellij-based terminal interface
4. Provides real-time monitoring and human-in-the-loop interaction
5. Handles conflict detection and coordinated merging

## Current Status

**Development Progress: 25/25 Tasks Complete (100%)** 🎉

### ✅ Completed Components (Session Progress)

#### Foundation (Tasks 1-3, 7)
1. **Project Setup** - SBCL, Quicklisp, ASDF with all dependencies
2. **Configuration** - YAML config, env vars, API key management
3. **Task Decomposition** - Taskmaster CLI integration
4. **Git Worktrees** - Isolated development environments

#### Enhanced Features (Tasks 4-6)
5. **PRD Parsing** - Multi-format support (Markdown, YAML, Plain Text)
   - AI-enhanced analysis with Claude
   - Quality assessment and ambiguity resolution
   
6. **Intelligent Tagging** - Domain-specific task categorization
   - AI-powered tagging (ui, backend, database, etc.)
   - Parallel execution grouping
   - Dependency-aware planning
   
7. **Subtask Expansion** - Hierarchical decomposition (2-3 levels)
   - Claude API for atomic subtask generation
   - Coverage validation
   - Complexity analysis

#### Orchestration (Tasks 8, 10, 14)
8. **Sub-Agent Spawning** - Zellij terminal multiplexer integration
   - Tab creation and management
   - Claude Code agent spawning
   - Context file generation
   
9. **Real-Time Dashboard** - ANSI terminal UI
   - Progress bars and status indicators
   - Flicker-free differential rendering
   - Background update thread
   
10. **Human-in-the-Loop REPL** - Interactive command interface
    - Status monitoring and agent control
    - Direct agent communication
    - Command history and validation

#### Core Infrastructure (Tasks 9, 11)
11. **Agent-Computer Interface (ACI)** - Tool definitions for agents
    - 8 core tools (git, filesystem, task management)
    - JSON schema validation
    - Safety and command whitelisting
    
12. **Agent Status Protocol** - JSON-based communication
    - Status file reading/writing
    - Change detection and polling
    - Monitoring loop with callbacks

### ✅ ALL TASKS COMPLETE!

**Core Infrastructure:** ✅ 100%
- Tasks 1-3, 7: Foundation (config, decomposition, git)
- Tasks 9, 11: Communication (ACI, status protocol)

**Enhanced Features:** ✅ 100%
- Tasks 4-6: Parsing, tagging, expansion

**Orchestration Platform:** ✅ 100%
- Tasks 8, 10, 14-17: Agent spawning, dashboard, REPL, checkpoints

**Intelligence & Automation:** ✅ 100%
- Tasks 12-13, 18-20: AI analysis, conflicts, PR workflow

**Infrastructure:** ✅ 100%
- Tasks 21-25: Performance, reliability, security, usability, maintainability

## Features

- **Task Decomposition**: Automatically break down PRDs into structured tasks using Claude API
- **Multi-Agent Coordination**: Spawn and manage multiple Claude Code instances in parallel
- **Git Worktree Management**: Isolated development environments for each agent group
- **Configuration System**: YAML-based configuration with environment variable support
- **Taskmaster Integration**: CLI-based task decomposition and management
- **Real-Time Monitoring**: Terminal-based dashboard showing agent progress
- **Human-in-the-Loop**: Interactive REPL interface for oversight and control
- **Conflict Detection**: Proactive identification of merge conflicts and dependencies
- **Pull Request Management**: Automated PR creation with rich context

## Requirements

- SBCL (Steel Bank Common Lisp) >= 2.5.9
- Quicklisp package manager
- Task Master CLI (for task decomposition)
- Zellij terminal multiplexer (for agent spawning)
- Claude API access (for AI-powered features)

## Installation

```bash
# Install dependencies
brew install sbcl

# Load the system
sbcl --eval "(ql:quickload :paos)"
```

## Usage

### Basic Usage

```lisp
;; Load configuration
(paos/core:load-config)

;; Decompose a PRD
(paos/core:decompose-prd "path/to/prd.txt" :num-tasks 5 :tag "feature-x")

;; Create a worktree for an agent
(paos/core:create-worktree "ui-agent")

;; List all worktrees
(paos/core:list-worktrees)
```

### Configuration

Create `.paos/config.yaml`:

```yaml
# API Keys (can be overridden by environment variables)
api_keys:
  anthropic: ""  # ANTHROPIC_API_KEY env var
  openai: ""     # OPENAI_API_KEY env var

# Worktree Configuration
worktree:
  root: ".paos/worktrees"

# Agent Configuration
agents:
  max_concurrent: 5
  timeout: 3600
```

## Architecture

The system consists of several key components:

### Core Modules (`core/`)

- **`package.lisp`**: Package definitions and exports
- **`config.lisp`**: Configuration management and validation
- **`decomposer.lisp`**: Taskmaster CLI integration for PRD decomposition
- **`git.lisp`**: Git worktree management and branch handling

### Planned Modules (`src/`)

- **`taskmaster.lisp`**: Enhanced Taskmaster integration
- **`prd-parser.lisp`**: Multi-format PRD parsing
- **`tagger.lisp`**: Intelligent task tagging with Claude
- **`expander.lisp`**: Subtask expansion
- **`zellij.lisp`**: Terminal multiplexer integration
- **`dashboard.lisp`**: Real-time status UI
- **`orchestrator.lisp`**: Main orchestration logic
- **`monitor.lisp`**: Agent status monitoring

### Integrations (`integrations/`)

- **`github.lisp`**: Pull request management
- **`review.lisp`**: Code review assistance
- **`merge.lisp`**: Merge coordination

## Development

This project uses Task Master for project management. See `.taskmaster/` directory for task definitions and status.

### Current Task Progress

```bash
task-master list  # View current tasks
task-master show <id>  # View task details
task-master next  # Get next task to work on
```

### Development Workflow

1. **Task Planning**: Use Taskmaster to decompose PRDs into structured tasks
2. **Agent Spawning**: Create isolated Git worktrees for each agent
3. **Parallel Development**: Launch Claude Code instances in Zellij tabs
4. **Monitoring**: Real-time dashboard and status tracking
5. **Conflict Resolution**: Automated conflict detection and resolution
6. **Merge Coordination**: Dependency-aware merging and PR creation

## Testing

```bash
# Load and test the system
sbcl --eval "(ql:quickload :paos)" --eval "(paos/core:load-config)" --eval "(print \"PAOS loaded successfully\")"
```

## Contributing

1. Check current task status: `task-master list`
2. Pick a task: `task-master next`
3. Implement the functionality
4. Update task status: `task-master set-status --id=<id> --status=done`
5. Commit changes with descriptive messages

## License

MIT License - see LICENSE file for details.

