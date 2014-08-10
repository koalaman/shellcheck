% SHELLCHECK(1) Shell script analysis tool

# NAME

shellcheck - Shell script analysis tool

# SYNOPSIS

**shellcheck** [*OPTIONS*...] *FILES*...

# DESCRIPTION

ShellCheck is a static analysis and linting tool for sh/bash scripts. It's
mainly focused on handling typical beginner and intermediate level syntax
errors and pitfalls where the shell just gives a cryptic error message or
strange behavior, but it also reports on a few more advanced issues where
corner cases can cause delayed failures.

# OPTIONS

**-e**\ *CODE1*[,*CODE2*...],\ **--exclude=***CODE1*[,*CODE2*...]

:   Explicitly exclude the specified codes from the report. Subsequent **-e**
    options are cumulative, but all the codes can be specified at once,
    comma-separated as a single argument.

**-f** *FORMAT*, **--format=***FORMAT*

:   Specify the output format of shellcheck, which prints its results in the
    standard output. Subsequent **-f** options are ignored, see **FORMATS**
    below for more information.

**-s**\ *shell*,\ **--shell=***shell*

:   Specify Bourne shell dialect. Valid values are *sh*, *bash*, *ksh* and
    *zsh*. The default is to use the file's shebang, or *bash* if the target
    shell can't be determined.

**-V**\ *version*,\ **--version**

:   Print version and exit.

# FORMATS

**tty**

:   Plain text, human readable output. This is the default.

**gcc**

:   GCC compatible output. Useful for editors that support compiling and
    showing syntax errors.

    For example, in Vim, `:set makeprg=shellcheck\ -f\ gcc\ %` will allow
    using `:make` to check the script, and `:cnext` to jump to the next error.

        <file>:<line>:<column>: <type>: <message>

**checkstyle**

:   Checkstyle compatible XML output. Supported directly or through plugins
    by many IDEs and build monitoring systems.

        <?xml version='1.0' encoding='UTF-8'?>
        <checkstyle version='4.3'>
          <file name='file'>
            <error
              line='line'
              column='column'
              severity='severity'
              message='message'
              source='ShellCheck.SC####' />
            ...
          </file>
          ...
        </checkstyle>

**json**

:   Json is a popular serialization format that is more suitable for web
    applications. ShellCheck's json is compact and contains only the bare
    minimum.

        [
          {
            "line": line,
            "column": column,
            "level": level,
            "code": ####,
            "message": message
          },
          ...
        ]

# DIRECTIVES
ShellCheck directives can be specified as comments in the shell script
before a command or block:

    # shellcheck key=value key=value
    command-or-structure

For example, to suppress SC2035 about using `./*.jpg`:

    # shellcheck disable=SC2035
    echo "Files: " *.jpg

Valid keys are:

**disable**
:   Disables a comma separated list of error codes for the following command.
    The command can be a simple command like `echo foo`, or a compound command
    like a function definition, subshell block or loop.


# AUTHOR
ShellCheck is written and maintained by Vidar Holen.

# REPORTING BUGS
Bugs and issues can be reported on GitHub:

https://github.com/koalaman/shellcheck/issues

# SEE ALSO

sh(1) bash(1)
