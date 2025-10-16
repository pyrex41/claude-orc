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
   #:parse-markdown-prd
   #:parse-plain-text-prd
   #:parse-yaml-prd
   #:extract-tasks))

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
   #:zellij-tab-new
   #:zellij-tab-switch
   #:zellij-tab-close
   #:zellij-session-info))

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
   #:move-cursor))

(defpackage #:paos/status
  (:use #:cl #:paos/core)
  (:export
   #:agent-status
   #:write-json-status
   #:read-json-status
   #:status-changed-p))

(defpackage #:paos/ai
  (:use #:cl #:paos/core)
  (:export
   #:call-claude
   #:analyze-statuses
   #:generate-recommendations))

(defpackage #:paos/conflicts
  (:use #:cl #:paos/core)
  (:export
   #:scan-overlaps
   #:detect-conflicts
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
   #:create-pr
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
   #:auto-save-thread
   #:load-saved-state
   #:backup-state
   #:recover-session))

(defpackage #:paos/usability
  (:use #:cl #:paos/core)
  (:export
   #:help-system
   #:improve-error-messages
   #:command-discovery))

(defpackage #:paos/security
  (:use #:cl #:paos/core)
  (:export
   #:encrypt-api-keys
   #:audit-log
   #:sanitize-prompts
   #:setup-sandbox))

;; Main PAOS package
(defpackage #:paos
  (:use #:cl)
  (:export
   #:main
   #:start
   #:stop))
