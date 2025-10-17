;;;; PAOS - PRD Parser Tests
;;;; Test suite for multi-format PRD parsing

(defpackage #:paos/test-parsers
  (:use #:cl #:paos/prd-parser)
  (:export #:run-parser-tests))

(in-package #:paos/test-parsers)

;;; Test data for parsers

(defparameter *test-markdown-content*
  "# Test PRD

## Overview
This is a test PRD for the system.

## Requirements
1. Implement user authentication
2. Create dashboard
3. Build API endpoints

## Features
- Must support OAuth2
- Should have rate limiting
- Will include analytics")

(defparameter *test-plain-text-content*
  "Test PRD for System

This PRD describes requirements for a new system.

1. Implement database schema
2. Create REST API
3. Build user interface

The system should handle 1000 concurrent users.
Must implement proper error handling.
Will support multiple languages.")

(defparameter *test-yaml-content*
  "overview: Test PRD for API Gateway

tasks:
  - title: Setup authentication
    description: Implement JWT auth
    priority: high
    
  - title: Create endpoints
    description: Build REST endpoints
    priority: medium")

;;; Test functions

(defun test-markdown-parser ()
  "Test Markdown PRD parser."
  (format t "~%Testing Markdown Parser...~%")
  (let ((result (paos/prd-parser::extract-markdown-structure *test-markdown-content*)))
    (format t "Format: ~A~%" (gethash "format" result))
    (format t "Tasks found: ~A~%" (length (gethash "tasks" result)))
    (format t "Metadata: ~A~%~%" (hash-table-count (gethash "metadata" result)))
    result))

(defun test-plain-text-parser ()
  "Test Plain Text PRD parser."
  (format t "~%Testing Plain Text Parser...~%")
  (let ((result (paos/prd-parser::extract-plain-text-structure *test-plain-text-content*)))
    (format t "Format: ~A~%" (gethash "format" result))
    (format t "Tasks found: ~A~%" (length (gethash "tasks" result)))
    result))

(defun test-yaml-parser ()
  "Test YAML PRD parser (requires cl-yaml)."
  (format t "~%Testing YAML Parser...~%")
  (handler-case
      (let ((yaml-data (cl-yaml:parse *test-yaml-content*)))
        (let ((result (paos/prd-parser::convert-yaml-to-tasks yaml-data "test.yaml")))
          (format t "Format: ~A~%" (gethash "format" result))
          (format t "Tasks found: ~A~%" (length (gethash "tasks" result)))
          result))
    (error (e)
      (format t "YAML parsing error: ~A~%" e)
      nil)))

(defun test-format-detection ()
  "Test PRD format detection."
  (format t "~%Testing Format Detection...~%")
  (flet ((test-detect (path)
           (format t "~A -> ~A~%"
                   path
                   (paos/prd-parser::detect-prd-format path))))
    (test-detect "test.md")
    (test-detect "test.txt")
    (test-detect "test.yaml")
    (test-detect "test.yml")
    (test-detect "test.unknown")))

(defun run-parser-tests ()
  "Run all PRD parser tests."
  (format t "~%=== PAOS PRD Parser Test Suite ===~%")
  (test-markdown-parser)
  (test-plain-text-parser)
  (test-yaml-parser)
  (test-format-detection)
  (format t "~%=== Tests Complete ===~%"))

;;; Manual test with actual files

(defun test-with-sample-files ()
  "Test parsers with actual sample files."
  (format t "~%Testing with sample files...~%")
  
  ;; Test Markdown
  (when (probe-file "samples/sample-prd.md")
    (format t "~%Parsing samples/sample-prd.md...~%")
    (let ((result (parse-markdown-prd "samples/sample-prd.md")))
      (format t "  Tasks: ~A~%" (length (extract-tasks result)))))
  
  ;; Test YAML
  (when (probe-file "samples/sample-prd.yaml")
    (format t "~%Parsing samples/sample-prd.yaml...~%")
    (let ((result (parse-yaml-prd "samples/sample-prd.yaml")))
      (format t "  Tasks: ~A~%" (length (extract-tasks result)))))
  
  ;; Test plain text (existing sample)
  (when (probe-file "sample-prd.txt")
    (format t "~%Parsing sample-prd.txt...~%")
    (let ((result (parse-plain-text-prd "sample-prd.txt")))
      (format t "  Tasks: ~A~%" (length (extract-tasks result))))))
