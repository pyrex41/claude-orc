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

**Development Progress: 4/25 Tasks Complete (16%)**

### ✅ Completed Components

1. **Project Setup and Dependencies** - Complete
   - SBCL 2.5.9 installed and configured
   - Quicklisp package manager installed
   - ASDF system definition with comprehensive dependencies
   - Package structure established

2. **Configuration Management** - Complete
   - YAML configuration file loading (.paos/config.yaml)
   - Environment variable integration (ANTHROPIC_API_KEY, etc.)
   - Secure API key handling
   - Configuration validation

3. **Task Decomposition Engine** - Complete
   - Taskmaster CLI integration
   - PRD parsing and YAML output processing
   - Error handling and fallback mechanisms
   - Structured task extraction

4. **Git Worktree Management** - Complete
   - Worktree creation with proper naming conventions
   - Branch management and isolation
   - Worktree listing and status checking
   - Path formatting and validation

### 🚧 In Development

- **Sub-Agent Spawning**: Zellij integration for Claude Code instances
- **PRD Parsing**: Multi-format support (Markdown, Plain Text, YAML)
- **Agent-Computer Interface**: Tool definitions following Anthropic practices
- **Real-Time Dashboard**: ANSI escape code-based UI
- **Status Protocol**: JSON-based agent communication
- **Orchestrator Intelligence**: LLM-based status analysis
- **Conflict Detection**: File overlap and dependency analysis
- **Human-in-the-Loop Interface**: Interactive REPL
- **Pull Request Management**: GitHub CLI integration
- **Security & Reliability**: Encryption, audit trails, state persistence

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

