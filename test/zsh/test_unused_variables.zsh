#!/bin/zsh
# Test: Unused variables

unused_var="never used"  # SC2034: unused_var appears unused

used_var="this is used"
echo "$used_var"

# Unused in function
function test_func() {
    local unused_local="not used"  # SC2034: unused_local appears unused
    local used_local="used"
    echo "$used_local"
}

test_func
