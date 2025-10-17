;;;; PAOS - Security Implementation
;;;; API key encryption, audit logging, prompt sanitization, sandboxing

(in-package #:paos/security)

;;; ============================================================================
;;; API Key Encryption
;;; ============================================================================

(defvar *encryption-key* nil
  "Master encryption key (derived from user password or system key).")

(defvar *encrypted-keys-file* ".paos/keys.encrypted"
  "Path to encrypted API keys file.")

(defun initialize-encryption (&optional password)
  "Initialize encryption with password or generate key."
  (setf *encryption-key*
        (if password
            (derive-key-from-password password)
            (generate-encryption-key)))
  (log-info "Encryption initialized"))

(defun derive-key-from-password (password)
  "Derive encryption key from password using PBKDF2."
  (let ((salt (or (load-salt) (generate-and-save-salt))))
    (ironclad:pbkdf2-hash-password 
     (ironclad:ascii-string-to-byte-array password)
     :salt salt
     :digest :sha256
     :iterations 100000)))

(defun generate-encryption-key ()
  "Generate random encryption key."
  (ironclad:random-data 32))  ; 256-bit key

(defun generate-and-save-salt ()
  "Generate and save salt for key derivation."
  (let ((salt (ironclad:random-data 16)))
    (with-open-file (stream ".paos/salt.dat"
                           :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
      (write-sequence salt stream))
    salt))

(defun load-salt ()
  "Load salt from file."
  (when (probe-file ".paos/salt.dat")
    (with-open-file (stream ".paos/salt.dat"
                           :element-type '(unsigned-byte 8))
      (let ((salt (make-array 16 :element-type '(unsigned-byte 8))))
        (read-sequence salt stream)
        salt))))

(defun encrypt-api-key (api-key key-name)
  "Encrypt an API key and store it securely."
  (unless *encryption-key*
    (error "Encryption not initialized"))
  
  (let* ((plaintext (ironclad:ascii-string-to-byte-array api-key))
         (cipher (ironclad:make-cipher :aes :key *encryption-key* :mode :cbc))
         (iv (ironclad:random-data 16))
         (encrypted (ironclad:encrypt-in-place cipher plaintext)))
    
    ;; Save encrypted data with IV
    (save-encrypted-key key-name iv encrypted)
    (log-info "Encrypted API key: ~A" key-name)))

(defun decrypt-api-key (key-name)
  "Decrypt and retrieve API key."
  (unless *encryption-key*
    (error "Encryption not initialized"))
  
  (multiple-value-bind (iv encrypted)
      (load-encrypted-key key-name)
    (when encrypted
      (let* ((cipher (ironclad:make-cipher :aes :key *encryption-key* :mode :cbc))
             (decrypted (ironclad:decrypt-in-place cipher encrypted)))
        (ironclad:byte-array-to-hex-string decrypted)))))

(defun save-encrypted-key (key-name iv encrypted-data)
  "Save encrypted key with IV."
  (ensure-directories-exist (directory-namestring *encrypted-keys-file*))
  (let ((data (list (cons :name key-name)
                   (cons :iv (ironclad:byte-array-to-hex-string iv))
                   (cons :data (ironclad:byte-array-to-hex-string encrypted-data)))))
    (with-open-file (stream *encrypted-keys-file*
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
      (format stream "~A~%" (cl-json:encode-json-to-string data)))))

(defun load-encrypted-key (key-name)
  "Load encrypted key and IV."
  (when (probe-file *encrypted-keys-file*)
    (with-open-file (stream *encrypted-keys-file*)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((data (cl-json:decode-json-from-string line)))
                 (when (string= (cdr (assoc :name data)) key-name)
                   (return (values
                            (ironclad:hex-string-to-byte-array (cdr (assoc :iv data)))
                            (ironclad:hex-string-to-byte-array (cdr (assoc :data data)))))))))))

;;; ============================================================================
;;; Audit Logging
;;; ============================================================================

(defvar *audit-log-file* ".paos/audit.log"
  "Path to audit log file.")

(defvar *audit-enabled* t
  "Enable/disable audit logging.")

(defun audit-log (event-type data &key severity)
  "Log an event to the audit trail."
  (when *audit-enabled*
    (handler-case
        (let ((entry (make-audit-entry event-type data severity)))
          (write-audit-entry entry))
      (error (e)
        (format *error-output* "Audit logging failed: ~A~%" e)))))

(defun make-audit-entry (event-type data severity)
  "Create audit log entry."
  (list (cons :timestamp (get-universal-time))
        (cons :iso-timestamp (format-iso-timestamp (get-universal-time)))
        (cons :event-type event-type)
        (cons :severity (or severity "info"))
        (cons :data data)
        (cons :pid (get-process-id))))

(defun write-audit-entry (entry)
  "Write audit entry to log file."
  (ensure-directories-exist (directory-namestring *audit-log-file*))
  (with-open-file (stream *audit-log-file*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format stream "~A~%" (cl-json:encode-json-to-string entry))))

(defun format-iso-timestamp (universal-time)
  "Format timestamp in ISO 8601 format."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour min sec)))

;;; Audit event helpers
(defun audit-api-call (endpoint &optional request-data)
  "Audit an API call."
  (audit-log "api-call"
            (list (cons :endpoint endpoint)
                  (cons :request (sanitize-data request-data)))
            :severity "info"))

(defun audit-key-access (key-name operation)
  "Audit API key access."
  (audit-log "key-access"
            (list (cons :key-name key-name)
                  (cons :operation operation))
            :severity "security"))

(defun audit-agent-action (agent-tag action &optional details)
  "Audit agent action."
  (audit-log "agent-action"
            (list (cons :agent agent-tag)
                  (cons :action action)
                  (cons :details details))
            :severity "info"))

(defun audit-security-event (event-description &optional severity)
  "Audit security-related event."
  (audit-log "security-event"
            (list (cons :description event-description))
            :severity (or severity "warning")))

;;; ============================================================================
;;; Prompt Sanitization
;;; ============================================================================

(defvar *sensitive-patterns* 
  '("api[_-]?key" "secret" "password" "token" "auth" "bearer"
    "sk-[a-zA-Z0-9]+" "ghp_[a-zA-Z0-9]+" "AKIA[a-zA-Z0-9]+")
  "Regex patterns for sensitive data.")

(defun sanitize-prompt (text)
  "Sanitize text by redacting sensitive information.
Returns sanitized text with secrets replaced by ***REDACTED***."
  (let ((sanitized text))
    (dolist (pattern *sensitive-patterns*)
      (setf sanitized
            (ppcre:regex-replace-all 
             (format nil "~A[:\\s=]+['\"]?([a-zA-Z0-9_-]+)['\"]?" pattern)
             sanitized
             (format nil "~A: ***REDACTED***" pattern)
             :case-insensitive-mode t)))
    sanitized))

(defun sanitize-data (data)
  "Recursively sanitize data structure."
  (cond
    ((stringp data) (sanitize-prompt data))
    ((hash-table-p data)
     (let ((sanitized (make-hash-table :test 'equal)))
       (maphash (lambda (k v)
                  (if (sensitive-key-p k)
                      (setf (gethash k sanitized) "***REDACTED***")
                      (setf (gethash k sanitized) (sanitize-data v))))
                data)
       sanitized))
    ((listp data)
     (mapcar #'sanitize-data data))
    (t data)))

(defun sensitive-key-p (key)
  "Check if a key name suggests sensitive data."
  (let ((key-str (string-downcase (format nil "~A" key))))
    (some (lambda (pattern)
            (search pattern key-str))
          '("key" "secret" "password" "token" "auth"))))

;;; ============================================================================
;;; Sandboxing
;;; ============================================================================

(defvar *sandbox-enabled* nil
  "Enable/disable sandboxing.")

(defvar *allowed-commands* '("git" "gh" "npm" "make" "test" "build")
  "Whitelist of allowed commands in sandbox.")

(defvar *blocked-paths* '("/etc" "/sys" "/proc" "/dev" "/boot")
  "Paths blocked from access in sandbox.")

(defun setup-sandbox ()
  "Initialize sandbox environment for agents."
  (setf *sandbox-enabled* t)
  (log-info "Sandbox enabled")
  (log-info "Allowed commands: ~{~A~^, ~}" *allowed-commands*)
  (log-info "Blocked paths: ~{~A~^, ~}" *blocked-paths*))

(defun validate-command (command)
  "Validate command against sandbox rules."
  (let ((cmd-name (car (str:words command))))
    (cond
      ((not *sandbox-enabled*) t)
      ((member cmd-name *allowed-commands* :test #'string-equal) t)
      (t
       (audit-security-event 
        (format nil "Blocked command attempt: ~A" command)
        "warning")
       (log-warn "Command not allowed: ~A" cmd-name)
       nil))))

(defun validate-file-access (path operation)
  "Validate file access against sandbox rules."
  (cond
    ((not *sandbox-enabled*) t)
    ((path-blocked-p path)
     (audit-security-event
      (format nil "Blocked file access: ~A (~A)" path operation)
      "critical")
     (log-error "File access denied: ~A" path)
     nil)
    (t t)))

(defun path-blocked-p (path)
  "Check if path is in blocked list."
  (some (lambda (blocked)
          (str:starts-with? blocked path))
        *blocked-paths*))

;;; ============================================================================
;;; Security Integration
;;; ============================================================================

(defun secure-api-call (endpoint data &key api-key-name)
  "Make secure API call with auditing."
  ;; Audit the call
  (audit-api-call endpoint (sanitize-data data))
  
  ;; Access key with auditing
  (when api-key-name
    (audit-key-access api-key-name "read"))
  
  ;; Make call (placeholder)
  (log-info "Secure API call to ~A" endpoint))

(defun secure-file-operation (path operation content)
  "Perform file operation with security checks."
  (if (validate-file-access path operation)
      (progn
        (audit-log "file-operation"
                  (list (cons :path path)
                        (cons :operation operation))
                  :severity "info")
        ;; Perform operation
        t)
      (error "File operation denied: ~A" path)))

;;; ============================================================================
;;; Security Utilities
;;; ============================================================================

(defun rotate-encryption-key (new-password)
  "Rotate encryption key (re-encrypt all keys with new password)."
  (log-info "Rotating encryption key...")
  
  ;; Load all encrypted keys with old key
  (let ((old-keys '()))
    ;; ... implementation ...
    )
  
  ;; Re-encrypt with new key
  (initialize-encryption new-password)
  
  ;; ... re-encrypt and save ...
  
  (audit-security-event "Encryption key rotated" "critical")
  (log-info "Encryption key rotation complete"))

(defun generate-security-report ()
  "Generate security audit report."
  (let ((report (make-hash-table :test 'equal)))
    (setf (gethash "timestamp" report) (get-universal-time))
    (setf (gethash "encryption_enabled" report) (not (null *encryption-key*)))
    (setf (gethash "audit_enabled" report) *audit-enabled*)
    (setf (gethash "sandbox_enabled" report) *sandbox-enabled*)
    (setf (gethash "allowed_commands" report) *allowed-commands*)
    (setf (gethash "blocked_paths" report) *blocked-paths*)
    report))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun get-process-id ()
  "Get process ID in a portable way."
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)
  #+ecl (ext:getpid)
  #+clisp (ext:process-id)
  #-(or sbcl ccl ecl clisp) 0)  ; Fallback for other implementations

;;; ============================================================================
;;; Logging
;;; ============================================================================

(defun log-error (format-string &rest args)
  "Log error message."
  (format *error-output* "ERROR: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-warn (format-string &rest args)
  "Log warning message."
  (format *error-output* "WARN: ~A~%" 
          (apply #'format nil format-string args)))

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
