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

ShellCheck gives shell specific advice. Consider this line:

    (( area = 3.14*r*r ))

+ For scripts starting with `#!/bin/sh` (or when using `-s sh`), ShellCheck
will warn that `(( .. ))` is not POSIX compliant (similar to checkbashisms).

+ For scripts starting with `#!/bin/bash` (or using `-s bash`), ShellCheck
will warn that decimals are not supported.

+ For scripts starting with `#!/bin/ksh` (or using `-s ksh`), ShellCheck will
not warn at all, as `ksh` supports decimals in arithmetic contexts.

# OPTIONS

**-a**,\ **--check-sourced**

:   Emit warnings in sourced files. Normally, `shellcheck` will only warn
    about issues in the specified files. With this option, any issues in
    sourced files will also be reported.

**-C**[*WHEN*],\ **--color**[=*WHEN*]

:   For TTY output, enable colors *always*, *never* or *auto*. The default
    is *auto*. **--color** without an argument is equivalent to
    **--color=always**.

**-i**\ *CODE1*[,*CODE2*...],\ **--include=***CODE1*[,*CODE2*...]

:   Explicitly include only the specified codes in the report. Subsequent **-i**
    options are cumulative, but all the codes can be specified at once,
    comma-separated as a single argument. Include options override any provided
    exclude options.

**-e**\ *CODE1*[,*CODE2*...],\ **--exclude=***CODE1*[,*CODE2*...]

:   Explicitly exclude the specified codes from the report. Subsequent **-e**
    options are cumulative, but all the codes can be specified at once,
    comma-separated as a single argument.

**-f** *FORMAT*, **--format=***FORMAT*

:   Specify the output format of shellcheck, which prints its results in the
    standard output. Subsequent **-f** options are ignored, see **FORMATS**
    below for more information.

**--list-optional**

:   Output a list of known optional checks. These can be enabled with **-o**
    flags or **enable** directives.

**--norc**

:   Don't try to look for .shellcheckrc configuration files.

**-o**\ *NAME1*[,*NAME2*...],\ **--enable=***NAME1*[,*NAME2*...]

:   Enable optional checks. The special name *all* enables all of them.
    Subsequent **-o** options accumulate. This is equivalent to specifying
    **enable** directives.

**-P**\ *SOURCEPATH*,\ **--source-path=***SOURCEPATH*

:   Specify paths to search for sourced files, separated by `:` on Unix and
    `;` on Windows. This is equivalent to specifying `search-path`
    directives.

**-s**\ *shell*,\ **--shell=***shell*

:   Specify Bourne shell dialect. Valid values are *sh*, *bash*, *dash* and *ksh*.
    The default is to deduce the shell from the file's `shell` directive,
    shebang, or `.bash/.bats/.dash/.ksh` extension, in that order. *sh* refers to
    POSIX `sh` (not the system's), and will warn of portability issues.

**-S**\ *SEVERITY*,\ **--severity=***severity*

:   Specify minimum severity of errors to consider. Valid values in order of
    severity are *error*, *warning*, *info* and *style*.
    The default is *style*.

**-V**,\ **--version**

:   Print version information and exit.

**-W** *NUM*,\ **--wiki-link-count=NUM**

:   For TTY output, show *NUM* wiki links to more information about mentioned
    warnings. Set to 0 to disable them entirely.

**-x**,\ **--external-sources**

:   Follow `source` statements even when the file is not specified as input.
    By default, `shellcheck` will only follow files specified on the command
    line (plus `/dev/null`). This option allows following any file the script
    may `source`.

**FILES...**

:   One or more script files to check, or "-" for standard input.


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

**diff**

:   Auto-fixes in unified diff format. Can be piped to `git apply` or `patch -p1`
    to automatically apply fixes.

        --- a/test.sh
        +++ b/test.sh
        @@ -2,6 +2,6 @@
         ## Example of a broken script.
         for f in $(ls *.m3u)
         do
        -  grep -qi hq.*mp3 $f \
        +  grep -qi hq.*mp3 "$f" \
             && echo -e 'Playlist $f contains a HQ file in mp3 format'
         done


**json1**

:   Json is a popular serialization format that is more suitable for web
    applications. ShellCheck's json is compact and contains only the bare
    minimum.  Tabs are counted as 1 character.

        {
          comments: [
            {
              "file": "filename",
              "line": lineNumber,
              "column": columnNumber,
              "level": "severitylevel",
              "code": errorCode,
              "message": "warning message"
            },
            ...
          ]
        }

**json**

:   This is a legacy version of the **json1** format. It's a raw array of
    comments, and all offsets have a tab stop of 8.

**quiet**

:   Suppress all normal output. Exit with zero if no issues are found,
    otherwise exit with one. Stops processing after the first issue.


# DIRECTIVES

ShellCheck directives can be specified as comments in the shell script.
If they appear before the first command, they are considered file-wide.
Otherwise, they apply to the immediately following command or block:

    # shellcheck key=value key=value
    command-or-structure

For example, to suppress SC2035 about using `./*.jpg`:

    # shellcheck disable=SC2035
    echo "Files: " *.jpg

To tell ShellCheck where to look for an otherwise dynamically determined file:

    # shellcheck source=./lib.sh
    source "$(find_install_dir)/lib.sh"

Here a shell brace group is used to suppress a warning on multiple lines:

    # shellcheck disable=SC2016
    {
      echo 'Modifying $PATH'
      echo 'PATH=foo:$PATH' >> ~/.bashrc
    }

Valid keys are:

**disable**
:   Disables a comma separated list of error codes for the following command.
    The command can be a simple command like `echo foo`, or a compound command
    like a function definition, subshell block or loop.

**enable**
:   Enable an optional check by name, as listed with **--list-optional**.
    Only file-wide `enable` directives are considered.

**source**
:   Overrides the filename included by a `source`/`.` statement. This can be
    used to tell shellcheck where to look for a file whose name is determined
    at runtime, or to skip a source by telling it to use `/dev/null`.

**source-path**
:   Add a directory to the search path for `source`/`.` statements (by default,
    only ShellCheck's working directory is included). Absolute paths will also
    be rooted in these paths. The special path `SCRIPTDIR` can be used to
    specify the currently checked script's directory, as in
    `source-path=SCRIPTDIR` or `source-path=SCRIPTDIR/../libs`. Multiple
    paths accumulate, and `-P` takes precedence over them.

**shell**
:   Overrides the shell detected from the shebang.  This is useful for
    files meant to be included (and thus lacking a shebang), or possibly
    as a more targeted alternative to 'disable=2039'.

# RC FILES

Unless `--norc` is used, ShellCheck will look for a file `.shellcheckrc` or
`shellcheckrc` in the script's directory and each parent directory. If found,
it will read `key=value` pairs from it and treat them as file-wide directives.

Here is an example `.shellcheckrc`:

    # Look for 'source'd files relative to the checked script,
    # and also look for absolute paths in /mnt/chroot
    source-path=SCRIPTDIR
    source-path=/mnt/chroot

    # Turn on warnings for unquoted variables with safe values
    enable=quote-safe-variables

    # Turn on warnings for unassigned uppercase variables
    enable=check-unassigned-uppercase

    # Allow [ ! -z foo ] instead of suggesting -n
    disable=SC2236

If no `.shellcheckrc` is found in any of the parent directories, ShellCheck
will look in `~/.shellcheckrc` followed by the XDG config directory
(usually `~/.config/shellcheckrc`) on Unix, or `%APPDATA%/shellcheckrc` on
Windows. Only the first file found will be used.

Note for Snap users: the Snap sandbox disallows access to hidden files.
Use `shellcheckrc` without the dot instead.

Note for Docker users: ShellCheck will only be able to look for files that
are mounted in the container, so `~/.shellcheckrc` will not be read.


# ENVIRONMENT VARIABLES

The environment variable `SHELLCHECK_OPTS` can be set with default flags:

    export SHELLCHECK_OPTS='--shell=bash --exclude=SC2016'

Its value will be split on spaces and prepended to the command line on each
invocation.

# RETURN VALUES

ShellCheck uses the following exit codes:

+ 0: All files successfully scanned with no issues.
+ 1: All files successfully scanned with some issues.
+ 2: Some files could not be processed (e.g. file not found).
+ 3: ShellCheck was invoked with bad syntax (e.g. unknown flag).
+ 4: ShellCheck was invoked with bad options (e.g. unknown formatter).

# LOCALE

This version of ShellCheck is only available in English. All files are
leniently decoded as UTF-8, with a fallback of ISO-8859-1 for invalid
sequences. `LC_CTYPE` is respected for output, and defaults to UTF-8 for
locales where encoding is unspecified (such as the `C` locale).

Windows users seeing `commitBuffer: invalid argument (invalid character)`
should set their terminal to use UTF-8 with `chcp 65001`.

# AUTHORS

ShellCheck is developed and maintained by Vidar Holen, with assistance from a
long list of wonderful contributors.

# REPORTING BUGS

Bugs and issues can be reported on GitHub:

https://github.com/koalaman/shellcheck/issues

# COPYRIGHT

Copyright 2012-2019, Vidar Holen and contributors.
Licensed under the GNU General Public License version 3 or later,
see https://gnu.org/licenses/gpl.html

# SEE ALSO

sh(1) bash(1)
