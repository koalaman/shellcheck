# ZSH Test Suite for ShellCheck

This directory contains comprehensive test files for ShellCheck's ZSH support. Each file demonstrates specific issues that ShellCheck should detect.

## Test Files and Expected Issues

### test_unquoted_expansion.zsh
- **SC2086**: Unquoted variable expansions that can cause word splitting
- **SC2128**: Expanding arrays without index notation

### test_unused_variables.zsh
- **SC2034**: Variables that are assigned but never used

### test_undefined_variables.zsh
- **SC2154**: Variables that are referenced but never assigned

### test_forshort_tracking.zsh
- **Valid**: ZSH short for loops should track variables correctly
- No false positives for variables used after loops

### test_array_issues.zsh
- **SC2128**: Expanding an array without an index
- **SC2154**: Undefined array references

### test_command_not_found.zsh
- Tests for non-existent commands (may not produce warnings if commands exist on system)

### test_common_errors.zsh
- **SC2086**: Unquoted variables in arithmetic contexts
- **SC2100**: Incorrect arithmetic syntax (should use $((...)))
- **SC2071**: Using numeric comparison operators with strings

### test_quoting_issues.zsh
- **SC2086**: Unquoted variables in test conditions

### test_useless_cat.zsh
- **SC2002**: Useless use of cat in pipelines
- **SC2034**: Unused result variables

### test_param_flags_no_warn.zsh
- **Valid**: ZSH parameter flags should not trigger false positives
- **SC2154**: Only truly undefined variables should warn
- **SC2043**: Loop warnings for single-iteration loops

### test_glob_qualifiers_valid.zsh
- **Valid**: ZSH glob qualifiers are valid syntax (currently has parsing issues)

### test_anon_functions_valid.zsh
- **Valid**: ZSH anonymous functions are valid syntax (currently has parsing issues)

### test_loop_variable_reassignment.zsh
- **SC2165**: Loop variables being reassigned inside loops
- **SC2162**: read without -r flag

### test_test_operators.zsh
- **SC2331**: Using deprecated -a instead of -e
- **SC2166**: Using deprecated -o instead of ||
- **SC2086**: Unquoted variables in tests

### test_globbing_issues.zsh
- **SC2045**: Iterating over ls output (fragile)
- **SC2035**: Glob patterns that could be misinterpreted as options

### test_redirect_issues.zsh
- **SC2094**: Reading and writing the same file
- **SC2069**: Incorrect redirect order (2>&1 must be last)
- **SC2261**: Multiple redirects competing for same file descriptor

## Summary

**Total test files**: 18
**Total issues detected**: 40+ ShellCheck warnings/errors across all files
**Valid ZSH syntax tested**: Parameter flags, glob qualifiers, short for loops, anonymous functions

## Running Tests

To run ShellCheck on all test files:

```bash
cd /path/to/shellcheck
shellcheck test/zsh/*.zsh
```

To check specific issue codes:

```bash
shellcheck test/zsh/test_unused_variables.zsh  # Should show SC2034
shellcheck test/zsh/test_redirect_issues.zsh   # Should show SC2094, SC2069, SC2261
```

## Notes

- Some files contain valid ZSH syntax that ShellCheck currently has difficulty parsing (anonymous functions with arguments, glob qualifiers in traditional for loops)
- Test files are designed to trigger specific warnings while demonstrating both problematic and correct code
- Pattern match failures in AnalyzerLib.hs and Analytics.hs have been fixed to handle Zsh shell type
