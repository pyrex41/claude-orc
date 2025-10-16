# Technical Context

## Technologies Used

### Core Stack
- **SBCL 2.5.9**: Steel Bank Common Lisp implementation
- **Quicklisp**: Package manager for Common Lisp libraries
- **ASDF**: System definition facility for building the project

### Key Libraries
```lisp
:depends-on (
  :cl-json          ; JSON parsing/generation
  :dexador          ; HTTP client for API calls
  :bordeaux-threads ; Cross-platform threading
  :uiop             ; Utilities for external programs
  :cl-yaml          ; YAML parsing
  :cl-ppcre         ; Perl-compatible regex
  :local-time       ; Time handling
  :alexandria       ; Utility library
  :str              ; String manipulation
)
```

### External Tools
- **Taskmaster CLI**: AI-powered task decomposition (task-master-ai npm package)
- **Zellij**: Terminal multiplexer for agent management
- **Git**: Version control with worktree support
- **GitHub CLI** (gh): PR management
- **Claude API**: Anthropic's AI for analysis and generation

## Development Setup

### Installation
```bash
# Install SBCL
brew install sbcl  # macOS
# or apt-get install sbcl  # Linux

# Install Quicklisp (one-time setup)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit

# Install Zellij
brew install zellij  # macOS

# Install Taskmaster (requires Node.js)
npm install -g task-master-ai

# Install GitHub CLI
brew install gh
```

### Loading the System
```lisp
;; In SBCL REPL
(ql:quickload :paos)

;; Or from command line
sbcl --eval "(ql:quickload :paos)"
```

### Configuration Files
```yaml
# .paos/config.yaml
api_keys:
  anthropic: ""  # Use ANTHROPIC_API_KEY env var
  openai: ""     # Optional

worktree:
  root: ".paos/worktrees"

agents:
  max_concurrent: 5
  timeout: 3600
```

## Technical Constraints

### Performance
- **Target**: Dashboard refresh < 2 seconds
- **CPU Usage**: Keep under 5% during steady-state monitoring
- **Memory**: Each agent worktree ~50MB overhead

### Concurrency
- **Max Agents**: Configurable, default 5
- **Thread Safety**: Use locks for shared state access
- **Process Management**: Clean shutdown of all spawned processes

### Error Handling
- **External Processes**: Graceful degradation on CLI failures
- **API Calls**: Retry logic with exponential backoff
- **State Corruption**: Recovery from persisted state
- **Network Issues**: Local-first operation where possible

## Dependencies

### Critical Path
1. **SBCL** → Required for all Lisp code
2. **Quicklisp** → Required for library installation
3. **Git** → Required for worktree management
4. **Claude API Key** → Required for AI features

### Optional
- **Taskmaster CLI** → Has fallback decomposition logic
- **Zellij** → Could fall back to tmux
- **GitHub CLI** → Manual PR creation possible

### Development Dependencies
- **ASDF**: Build system
- **Quicklisp local-projects**: For development
- **SLIME/SLY**: Emacs integration (recommended)

## File Structure
```
paos/
├── paos.asd                 # ASDF system definition
├── core/                    # Core modules (✅ complete)
│   ├── package.lisp         # Package definitions
│   ├── config.lisp          # Configuration management
│   ├── decomposer.lisp      # Task decomposition
│   └── git.lisp             # Git worktree management
├── src/                     # Extended modules (planned)
│   ├── taskmaster.lisp      # Enhanced Taskmaster integration
│   ├── prd-parser.lisp      # Multi-format PRD parsing
│   ├── tagger.lisp          # Intelligent task tagging
│   ├── expander.lisp        # Subtask expansion
│   ├── zellij.lisp          # Terminal multiplexer integration
│   ├── dashboard.lisp       # Real-time status UI
│   ├── orchestrator.lisp    # Main orchestration logic
│   └── monitor.lisp         # Agent status monitoring
├── integrations/            # External integrations (planned)
│   ├── github.lisp          # Pull request management
│   ├── review.lisp          # Code review assistance
│   └── merge.lisp           # Merge coordination
├── .paos/                   # Runtime data
│   ├── config.yaml          # User configuration
│   ├── state.json           # Persisted state
│   └── worktrees/           # Agent worktrees
└── .taskmaster/             # Task management
    ├── tasks/tasks.json     # Task definitions
    └── config.json          # Taskmaster config
```

## Environment Variables
- `ANTHROPIC_API_KEY`: Required for Claude API
- `OPENAI_API_KEY`: Optional, for GPT models
- `PAOS_CONFIG_PATH`: Override config file location
- `PAOS_LOG_LEVEL`: debug|info|warn|error
