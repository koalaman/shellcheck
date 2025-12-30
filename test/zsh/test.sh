#!/bin/bash
cd /Users/agoodkind/Sites/shellcheck
cabal build 2>&1 | tail -10
echo "=== BUILD COMPLETE ==="
cabal test 2>&1 | grep -E "(prop_readZshAnonFunction|FAILED|PASSED)" | tail -20
