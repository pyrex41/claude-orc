;;;; PAOS - Parallel Agent Orchestration System
;;;; Shared AI utilities for Claude API integration

(in-package #:paos/core)

;;; Constants
(defparameter *claude-api-base* "https://api.anthropic.com/v1/messages"
  "Base URL for Claude API")

(defparameter *claude-model* "claude-3-5-sonnet-20241022"
  "Default Claude model to use")

;;; API Key Management

(defun get-api-key ()
  "Get the Anthropic API key from config or environment."
  (or (uiop:getenv "ANTHROPIC_API_KEY")
      (paos/core:api-key "anthropic")
      (error "ANTHROPIC_API_KEY not found in environment or config")))

;;; Claude API Integration

(defun call-claude-api (api-key prompt &key (model *claude-model*) (max-tokens 4096))
  "Make API call to Claude with the given prompt.
   Returns the raw HTTP response string."
  (let* ((headers `(("x-api-key" . ,api-key)
                   ("anthropic-version" . "2023-06-01")
                   ("content-type" . "application/json")))
         (body (cl-json:encode-json-to-string
                `(("model" . ,model)
                  ("max_tokens" . ,max-tokens)
                  ("messages" . ((("role" . "user")
                                 ("content" . ,prompt)))))))
         (response (dexador:post *claude-api-base*
                                :headers headers
                                :content body)))
    response))

(defun parse-claude-response (response-str)
  "Parse Claude API response JSON and extract the text content.
   Returns the assistant's message content as a string."
  (let* ((json (cl-json:decode-json-from-string response-str))
         (content (cdr (assoc :content json)))
         (first-block (if (listp content) (car content) content))
         (text (cdr (assoc :text first-block))))
    (or text
        (error "Failed to parse Claude response: no text content found"))))

(defun call-claude (prompt &key (model *claude-model*) (max-tokens 4096))
  "High-level function to call Claude and return the response text.
   Handles API key retrieval, API call, and response parsing."
  (let ((api-key (get-api-key)))
    (parse-claude-response
     (call-claude-api api-key prompt :model model :max-tokens max-tokens))))
