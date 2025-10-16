# Project Brief: PAOS (Parallel Agent Orchestration System)

## Overview
PAOS is a Common Lisp system for orchestrating multiple Claude Code agents to work on task decomposition and development workflows in parallel.

## Core Requirements

### Primary Goal
Build a terminal-based multi-agent orchestration platform that enables parallel development workflows by:
1. Parsing PRDs into structured tasks using AI
2. Spawning Claude Code instances in isolated Git worktrees
3. Managing agents through a Zellij-based terminal interface
4. Providing real-time monitoring and human-in-the-loop interaction
5. Handling conflict detection and coordinated merging

### Key Features
- **Task Decomposition**: Automatically break down PRDs into structured tasks using Claude API and Taskmaster CLI
- **Multi-Agent Coordination**: Spawn and manage multiple Claude Code instances in parallel
- **Git Worktree Management**: Isolated development environments for each agent group
- **Configuration System**: YAML-based configuration with environment variable support
- **Real-Time Monitoring**: Terminal-based dashboard showing agent progress
- **Human-in-the-Loop**: Interactive REPL interface for oversight and control
- **Conflict Detection**: Proactive identification of merge conflicts and dependencies
- **Pull Request Management**: Automated PR creation with rich context

## Technology Stack
- **Language**: Common Lisp (SBCL >= 2.5.9)
- **Package Manager**: Quicklisp
- **Task Management**: Taskmaster CLI
- **Terminal Multiplexer**: Zellij
- **AI**: Claude API (Anthropic)
- **Version Control**: Git with worktrees

## Project Scope

### In Scope
- Core task decomposition engine
- Multi-agent spawning and coordination
- Git worktree management
- Real-time status monitoring
- Human interaction interfaces
- Conflict detection and merge coordination
- Security (API key encryption, audit trails, sandboxing)

### Out of Scope (for now)
- Support for other AI providers beyond Claude
- Web-based UI (terminal-only for v1)
- Distributed/cloud deployment
- Multi-repository support

## Success Criteria
1. Successfully parse PRDs into structured tasks
2. Spawn multiple Claude Code agents in isolated worktrees
3. Monitor agent progress in real-time
4. Detect and resolve conflicts before merging
5. Maintain secure handling of API keys and sensitive data
6. Enable seamless human oversight and intervention

## Current Status
- **Development Progress**: 4/25 Tasks Complete (16%)
- **Phase**: Foundation complete, moving to enhanced features
