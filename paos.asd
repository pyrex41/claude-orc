;;;; PAOS - Parallel Agent Orchestration System
;;;; ASDF System Definition

(asdf:defsystem #:paos
  :description "Parallel Agent Orchestration System - A Common Lisp system for orchestrating multiple Claude Code agents"
  :author "PAOS Development Team"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-json
               #:dexador
               #:bordeaux-threads
               #:uiop
               #:cl-yaml
               #:cl-ppcre
               #:local-time
               #:alexandria
               #:str
               #:cl-base64
               #:ironclad
               #:cl-fad
               #:external-program
               #:cl-cron)
  :components ((:module "core"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "decomposer" :depends-on ("package" "config"))
                 (:file "git" :depends-on ("package" "config"))))
               (:module "src"
                :components
                ((:file "prd-parser")
                 (:file "ai-integration" :depends-on ("prd-parser"))
                 (:file "tagger")
                 (:file "expander")
                 (:file "zellij-integration")
                 (:file "dashboard")
                 (:file "repl"))
                :depends-on ("core"))))
  :in-order-to ((test-op (test-op #:paos/test))))

(asdf:defsystem #:paos/test
  :description "Test system for PAOS"
  :depends-on (#:paos #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :all-tests
                                                  :paos/test))))
