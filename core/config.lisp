;;;; PAOS - Configuration Management
;;;; Core configuration loading and validation

(in-package #:paos/core)

(defvar *config* nil
  "Global configuration object loaded from YAML and environment variables.")

(defvar *config-path* ".paos/config.yaml"
  "Default path to the configuration file.")

(defvar *required-fields* '("api_keys.anthropic"
                            "worktree.root"
                            "zellij.session_name"
                            "agents.max_concurrent"
                            "dashboard.update_interval"
                            "logging.level")
  "List of required configuration fields.")

(defun get-env-var (name &optional default)
  "Get environment variable value, returning DEFAULT if not found."
  (or (uiop:getenv name) default))

(defun load-config (&optional (config-path *config-path*))
  "Load configuration from YAML file and merge with environment variables.
Returns the merged configuration hash-table."
  (handler-case
      (let* ((pathname (pathname config-path))
             (yaml-config (if (probe-file pathname)
                              (yaml:parse pathname)
                              (make-hash-table :test #'equal)))
             (merged-config (merge-with-env-vars yaml-config)))
        (validate-config merged-config)
        (setf *config* merged-config)
        merged-config)
    (error (e)
      (error "Failed to load configuration from ~A: ~A" config-path e))))

(defun merge-with-env-vars (yaml-config)
  "Merge YAML configuration with environment variables.
Environment variables take precedence over YAML values."
  (let ((config (copy-hash-table yaml-config)))

    ;; API Keys - secure handling
    (when (gethash "api_keys" config)
      (let ((api-keys (gethash "api_keys" config)))
        (setf (gethash "anthropic" api-keys)
              (or (get-env-var "ANTHROPIC_API_KEY")
                  (gethash "anthropic" api-keys)))
        (setf (gethash "openai" api-keys)
              (or (get-env-var "OPENAI_API_KEY")
                  (gethash "openai" api-keys)))
        (setf (gethash "perplexity" api-keys)
              (or (get-env-var "PERPLEXITY_API_KEY")
                  (gethash "perplexity" api-keys)))
        (setf (gethash "github" api-keys)
              (or (get-env-var "GITHUB_TOKEN")
                  (gethash "github" api-keys)))))

    ;; Other environment variable overrides
    (setf (gethash "worktree.root" config)
          (or (get-env-var "PAOS_WORKTREE_ROOT")
              (gethash "worktree.root" config)))

    (setf (gethash "zellij.session_name" config)
          (or (get-env-var "PAOS_ZELLIJ_SESSION")
              (gethash "zellij.session_name" config)))

    config))

(defun validate-config (config)
  "Validate that all required configuration fields are present."
  (dolist (field-path *required-fields*)
    (unless (get-config-value field-path config)
      (error "Required configuration field missing: ~A" field-path)))

  ;; Validate API keys (at least one must be present)
  (let ((api-keys (get-config-value "api_keys" config)))
    (unless (or (gethash "anthropic" api-keys)
                (gethash "openai" api-keys))
      (error "At least one API key (anthropic or openai) must be configured")))

  ;; Validate numeric fields
  (let ((max-concurrent (get-config-value "agents.max_concurrent" config)))
    (unless (and (numberp max-concurrent) (> max-concurrent 0))
      (error "agents.max_concurrent must be a positive number")))

  (let ((update-interval (get-config-value "dashboard.update_interval" config)))
    (unless (and (numberp update-interval) (>= update-interval 1))
      (error "dashboard.update_interval must be >= 1")))

  t)

(defun get-config-value (path &optional (config *config*))
  "Get configuration value by dot-separated path (e.g., 'api_keys.anthropic').
Returns NIL if path doesn't exist."
  (let ((parts (split #\. path)))
    (labels ((get-nested (obj keys)
               (if (null keys)
                   obj
                   (let ((key (car keys)))
                     (typecase obj
                       (hash-table
                        (get-nested (gethash key obj) (cdr keys)))
                       (t nil))))))
      (get-nested config parts))))

(defun set-config-value (path value &optional (config *config*))
  "Set configuration value by dot-separated path."
  (let ((parts (split #\. path)))
    (labels ((set-nested (obj keys)
               (if (= 1 (length keys))
                   (setf (gethash (car keys) obj) value)
                   (let ((key (car keys))
                         (next-obj (or (gethash key obj)
                                       (setf (gethash key obj) (make-hash-table :test #'equal)))))
                     (set-nested next-obj (cdr keys))))))
      (set-nested config parts))))

(defun save-config (&optional (config-path *config-path*))
  "Save current configuration to YAML file.
Note: This will NOT save API keys for security reasons."
  (let ((config-copy (copy-hash-table *config* :deep t)))
    ;; Remove API keys before saving
    (when (gethash "api_keys" config-copy)
      (let ((api-keys (gethash "api_keys" config-copy)))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (setf (gethash k api-keys) ""))
                 api-keys)))
    (with-open-file (stream config-path :direction :output :if-exists :supersede)
      (yaml:emit config-copy stream))))

(defun ensure-config-loaded ()
  "Ensure configuration is loaded, loading it if necessary."
  (unless *config*
    (load-config)))

;; Convenience functions for common config access
(defun api-key (provider)
  "Get API key for the specified provider."
  (ensure-config-loaded)
  (get-config-value (format nil "api_keys.~A" provider)))

(defun worktree-root ()
  "Get the worktree root directory."
  (ensure-config-loaded)
  (get-config-value "worktree.root"))

(defun zellij-session-name ()
  "Get the Zellij session name."
  (ensure-config-loaded)
  (get-config-value "zellij.session_name"))

(defun max-concurrent-agents ()
  "Get the maximum number of concurrent agents."
  (ensure-config-loaded)
  (get-config-value "agents.max_concurrent"))

(defun dashboard-update-interval ()
  "Get the dashboard update interval in seconds."
  (ensure-config-loaded)
  (get-config-value "dashboard.update_interval"))

(defun log-level ()
  "Get the current log level."
  (ensure-config-loaded)
  (get-config-value "logging.level"))

;; Configuration for external dependencies
(defparameter *claude-config*
  '(:model "claude-3-5-sonnet-20241022"
    :max-tokens 4096
    :temperature 0.7
    :timeout 60)
  "Default Claude API configuration.")

(defun get-claude-config ()
  "Get Claude API configuration merged with user config."
  (ensure-config-loaded)
  (let ((user-config (get-config-value "claude")))
    (if user-config
        (list :model (or (gethash "model" user-config)
                         (getf *claude-config* :model))
              :max-tokens (or (gethash "max_tokens" user-config)
                              (getf *claude-config* :max-tokens))
              :temperature (or (gethash "temperature" user-config)
                               (getf *claude-config* :temperature))
              :timeout (or (gethash "timeout" user-config)
                           (getf *claude-config* :timeout)))
        *claude-config*)))

