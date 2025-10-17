#!/bin/bash
# PAOS Test Runner
# Runs the agent spawning test suite

set -e

echo "========================================="
echo "PAOS Agent Spawning Test Runner"
echo "========================================="
echo ""

# Check if Zellij is installed
if ! command -v zellij &> /dev/null; then
    echo "ERROR: Zellij not found!"
    echo "Install from: https://zellij.dev"
    exit 1
fi

echo "✓ Zellij detected: $(zellij --version)"
echo ""

# Run tests
echo "Loading PAOS system and running tests..."
echo ""

sbcl --noinform \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(asdf:load-system :paos)" \
     --eval "(load \"tests/test-agent-spawning.lisp\")" \
     --eval "(let ((success (paos/tests/spawning:run-all-tests)))
               (sb-ext:exit :code (if success 0 1)))"

echo ""
echo "Tests complete!"
