;;;; PAOS - Parallel Agent Orchestration System
;;;; Core package definitions

(defpackage #:paos/core
  (:use #:cl #:alexandria)
  (:shadowing-import-from #:str #:concat #:join #:split #:trim #:replace-all #:starts-with-p #:ends-with-p)
  (:export
   ;; Configuration
   #:load-config
   #:get-config-value
   #:validate-config
   #:api-key
   #:worktree-root
   #:zellij-session-name

   ;; AI Utilities
   #:get-api-key
   #:call-claude-api
   #:parse-claude-response
   #:call-claude
   #:*claude-api-base*
   #:*claude-model*

   ;; Orchestrator
   #:start-orchestrator
   #:stop-orchestrator
   #:orchestrator-status

   ;; Task Decomposition
   #:decompose-prd
   #:decompose-prd-with-fallback
   #:validate-decomposed-tasks
   #:extract-task-list

   ;; Git Worktree Management
   #:create-worktree
   #:remove-worktree
   #:list-worktrees
   #:switch-to-worktree
   #:get-worktree-branch
   #:initialize-git-worktrees
   #:format-worktree-path

   ;; Monitor
   #:start-monitor
   #:stop-monitor
   #:get-agent-statuses

   ;; Task decomposition
   #:decompose-prd
   #:parse-taskmaster-output

   ;; Agent management
   #:spawn-agent
   #:terminate-agent
   #:list-agents

   ;; Git worktree management
   #:create-worktree
   #:remove-worktree
   #:list-worktrees

   ;; Zellij integration
   #:create-zellij-tab
   #:switch-zellij-tab
   #:close-zellij-tab

   ;; Dashboard
   #:render-dashboard
   #:update-dashboard

   ;; Status protocol
   #:write-status-file
   #:read-status-file
   #:status-summary))

(defpackage #:paos/config
  (:use #:cl #:paos/core)
  (:export
   #:config
   #:config-path
   #:api-keys
   #:worktree-root
   #:zellij-session-name))

(defpackage #:paos/orchestrator
  (:use #:cl #:paos/core #:bordeaux-threads)
  (:export
   #:orchestrator
   #:run-orchestrator-loop
   #:process-commands
   #:handle-agent-status))

(defpackage #:paos/monitor
  (:use #:cl #:paos/core #:bordeaux-threads)
  (:export
   #:monitor
   #:start-polling
   #:stop-polling
   #:collect-statuses))

(defpackage #:paos/taskmaster
  (:use #:cl #:paos/core)
  (:export
   #:call-taskmaster
   #:parse-yaml-output
   #:decompose-with-taskmaster))

(defpackage #:paos/prd-parser
  (:use #:cl #:paos/core)
  (:export
   ;; Format-specific parsers
   #:parse-markdown-prd
   #:parse-plain-text-prd
   #:parse-yaml-prd
   
   ;; Auto-detection
   #:detect-prd-format
   #:parse-prd
   
   ;; Task extraction
   #:extract-tasks
   
   ;; AI-enhanced parsing
   #:parse-prd-with-ai
   #:analyze-prd-with-claude
   #:analyze-prd-quality
   #:resolve-ambiguous-requirements))

(defpackage #:paos/tagger
  (:use #:cl #:paos/core)
  (:export
   #:tag-tasks
   #:group-by-tags
   #:validate-tag-groups))

(defpackage #:paos/expander
  (:use #:cl #:paos/core)
  (:export
   #:expand-task
   #:create-subtasks
   #:validate-expansion))

(defpackage #:paos/git
  (:use #:cl #:paos/core)
  (:export
   #:git-worktree-add
   #:git-worktree-remove
   #:git-worktree-list
   #:git-branch-create))

(defpackage #:paos/zellij
  (:use #:cl #:paos/core)
  (:export
   ;; Tab management
   #:create-zellij-tab
   #:switch-zellij-tab
   #:close-zellij-tab
   #:list-zellij-tabs

   ;; Agent spawning
   #:spawn-agent
   #:send-to-tab
   #:spawn-agent-with-retry
   #:validate-spawn-result

   ;; Session management
   #:create-zellij-session
   #:attach-to-session
   #:session-exists-p
   #:get-session-info

   ;; Context management
   #:create-agent-context

   ;; Utilities
   #:check-zellij-installed
   #:format-tab-name))

(defpackage #:paos/aci
  (:use #:cl #:paos/core)
  (:export
   #:define-tool
   #:validate-tool-schema
   #:tool-schemas))

(defpackage #:paos/dashboard
  (:use #:cl #:paos/core)
  (:export
   #:ansi-dashboard
   #:render-progress-bar
   #:clear-screen
   #:move-cursor
   #:create-dashboard
   #:render-dashboard
   #:update-dashboard
   #:start-dashboard
   #:stop-dashboard
   #:colorize
   #:demo-dashboard
   ;; ANSI color codes
   #:*ansi-reset*
   #:*ansi-bold*
   #:*ansi-red*
   #:*ansi-green*
   #:*ansi-yellow*
   #:*ansi-blue*
   #:*ansi-magenta*
   #:*ansi-cyan*
   #:*ansi-white*))

(defpackage #:paos/status
  (:use #:cl #:paos/core)
  (:export
   ;; Class and constructor
   #:agent-status
   #:create-agent-status

   ;; Accessors
   #:status-tag
   #:status-state
   #:status-progress
   #:status-current-task
   #:status-completed-tasks
   #:status-errors
   #:status-last-update
   #:status-metadata

   ;; I/O operations
   #:write-agent-status
   #:read-agent-status
   #:update-agent-status
   #:write-json-status
   #:read-json-status

   ;; Status monitoring
   #:poll-agent-statuses
   #:poll-all-statuses
   #:status-changed-p
   #:get-changed-statuses

   ;; Aggregation and analysis
   #:summarize-statuses
   #:get-stale-agents

   ;; Monitoring loop
   #:start-status-monitor
   #:stop-status-monitor))

(defpackage #:paos/ai
  (:use #:cl #:paos/core)
  (:export
   ;; Analysis functions
   #:analyze-agent-statuses
   #:process-analysis-recommendations
   #:execute-recommendations
   #:display-analysis

   ;; Continuous analysis
   #:start-continuous-analysis
   #:stop-continuous-analysis
   #:get-latest-analysis

   ;; Legacy exports
   #:call-claude
   #:analyze-statuses
   #:generate-recommendations))

(defpackage #:paos/conflicts
  (:use #:cl #:paos/core)
  (:export
   ;; Conflict detection
   #:detect-conflicts
   #:find-file-overlaps
   #:find-dependency-conflicts
   #:detect-semantic-conflicts

   ;; Alert generation
   #:generate-conflict-alerts
   #:display-conflicts
   #:export-conflict-report

   ;; Continuous monitoring
   #:start-conflict-monitoring
   #:stop-conflict-monitoring

   ;; Legacy exports
   #:scan-overlaps
   #:generate-alerts))

(defpackage #:paos/human-interface
  (:use #:cl #:paos/core)
  (:export
   #:repl-loop
   #:process-command
   #:show-help
   #:format-status))

(defpackage #:paos/checkpoints
  (:use #:cl #:paos/core)
  (:export
   #:define-checkpoint
   #:pause-at-checkpoint
   #:resume-from-checkpoint))

(defpackage #:paos/github
  (:use #:cl #:paos/core)
  (:export
   ;; PR creation
   #:create-pr
   #:create-prs-for-worktrees
   #:format-pr-with-ai
   #:generate-pr-title
   #:generate-pr-body

   ;; PR management
   #:list-prs
   #:get-pr-status
   #:merge-pr

   ;; PR utilities
   #:get-changed-files
   #:extract-pr-dependencies

   ;; CLI utilities
   #:check-gh-cli

   ;; Legacy exports
   #:get-pr-diffs
   #:format-pr-body))

(defpackage #:paos/review
  (:use #:cl #:paos/core)
  (:export
   #:analyze-prs
   #:generate-review-insights
   #:suggest-merge-order))

(defpackage #:paos/merge
  (:use #:cl #:paos/core)
  (:export
   #:build-dependency-graph
   #:calculate-merge-order
   #:execute-merges
   #:handle-merge-conflicts))

(defpackage #:paos/performance
  (:use #:cl #:paos/core)
  (:export
   #:batch-status-requests
   #:cache-analyses
   #:measure-latency))

(defpackage #:paos/reliability
  (:use #:cl #:paos/core #:bordeaux-threads)
  (:export
   ;; Session state class and accessors
   #:session-state
   #:create-session-state
   #:state-agents
   #:state-worktrees
   #:state-config
   #:state-start-time
   #:state-last-save
   #:state-metadata

   ;; Auto-save
   #:start-auto-save
   #:stop-auto-save

   ;; State persistence
   #:save-session
   #:load-session
   #:backup-state
   #:clean-old-backups

   ;; Recovery
   #:recover-session
   #:recover-from-backup
   #:restart-agents

   ;; Session management
   #:update-session-agents
   #:update-session-worktrees

   ;; Crash handling
   #:setup-crash-handler
   #:safe-shutdown

   ;; Legacy exports
   #:auto-save-thread
   #:load-saved-state))

(defpackage #:paos/usability
  (:use #:cl #:paos/core)
  (:export
   #:help-system
   #:improve-error-messages
   #:command-discovery))

(defpackage #:paos/security
  (:use #:cl #:paos/core)
  (:export
   ;; Encryption
   #:initialize-encryption
   #:encrypt-api-key
   #:decrypt-api-key
   #:rotate-encryption-key

   ;; Audit logging
   #:audit-log
   #:audit-api-call
   #:audit-key-access
   #:audit-agent-action
   #:audit-security-event

   ;; Sanitization
   #:sanitize-prompt
   #:sanitize-data

   ;; Sandboxing
   #:setup-sandbox
   #:validate-command
   #:validate-file-access

   ;; Security integration
   #:secure-api-call
   #:secure-file-operation
   #:generate-security-report

   ;; Legacy exports
   #:encrypt-api-keys
   #:sanitize-prompts))

;; Main PAOS package
(defpackage #:paos
  (:use #:cl)
  (:export
   #:main
   #:start
   #:stop))
