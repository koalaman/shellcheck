# ShellCheck - A shell script static analysis tool

ShellCheck is a GPLv3 tool that gives warnings and suggestions for bash/sh shell scripts:

![Screenshot of a terminal showing problematic shell script lines highlighted](doc/terminal.png).

The goals of ShellCheck are

  - To point out and clarify typical beginner's syntax issues
    that cause a shell to give cryptic error messages.

  - To point out and clarify typical intermediate level semantic problems
    that cause a shell to behave strangely and counter-intuitively.

  - To point out subtle caveats, corner cases and pitfalls that may cause an
    advanced user's otherwise working script to fail under future circumstances.

See [the gallery of bad code](README.md#user-content-gallery-of-bad-code) for examples of what ShellCheck can help you identify!


## How to use
There are a variety of ways to use ShellCheck!


#### On the web
Paste a shell script on http://www.shellcheck.net for instant feedback.

[ShellCheck.net](http://www.shellcheck.net) is always synchronized to the latest git commit, and is the simplest way to give ShellCheck a go. Tell your friends!


#### From your terminal
Run `shellcheck yourscript` in your terminal for instant output, as seen above.


#### In your editor

You can see ShellCheck suggestions directly in a variety of editors.

* Vim, through [Syntastic](https://github.com/scrooloose/syntastic):

![Screenshot of vim showing inlined shellcheck feedback](doc/vim-syntastic.png).

* Emacs, through [Flycheck](https://github.com/flycheck/flycheck):

![Screenshot of emacs showing inlined shellcheck feedback](doc/emacs-flycheck.png).

* Sublime, through [SublimeLinter](https://github.com/SublimeLinter/SublimeLinter-shellcheck).

* Atom, through [Linter](https://github.com/AtomLinter/linter-shellcheck).

* Most other editors, through [GCC error compatibility](shellcheck.1.md#user-content-formats).


#### In your build or test suites
While ShellCheck is mostly intended for interactive use, it can easily be added to builds or test suites.

Use ShellCheck's exit code, or its [CheckStyle compatible XML output](shellcheck.1.md#user-content-formats). There's also a simple JSON output format for easy integration.


## Installing

The easiest way to install ShellCheck locally is through your package manager.

On systems with Cabal (installs to `~/.cabal/bin`):

    cabal update
    cabal install shellcheck

On Debian based distros:

    apt-get install shellcheck

On Gentoo based distros:

    emerge --ask shellcheck

On Fedora based distros:

    dnf install ShellCheck

On OS X with homebrew:

    brew install shellcheck

On OS X with MacPorts:

    port install shellcheck

On openSUSE:Tumbleweed:

    zypper in ShellCheck

On other openSUSE distributions:

add OBS devel:languages:haskell repository from https://build.opensuse.org/project/repositories/devel:languages:haskell

    zypper ar http://download.opensuse.org/repositories/devel:/languages:/haskell/openSUSE_$(version)/devel:languages:haskell.repo
    zypper in ShellCheck

or use OneClickInstall - https://software.opensuse.org/package/ShellCheck


## Compiling from source

This section describes how to build ShellCheck from a source directory. ShellCheck is written in Haskell and requires 2GB of RAM to compile.


#### Installing Cabal

ShellCheck is built and packaged using Cabal. Install the package `cabal-install` from your system's package manager (with e.g. `apt-get`, `yum`, `zypper` or `brew`).

On MacPorts, the package is instead called `hs-cabal-install`, while native Windows users should install the latest version of the Haskell platform from https://www.haskell.org/platform/

Verify that `cabal` is installed and update its dependency list with

    $ cabal update

#### Compiling ShellCheck

`git clone` this repository, and `cd` to the ShellCheck source directory to build/install:

    $ cabal install

This will compile ShellCheck and install it to your `~/.cabal/bin` directory.

Add this directory to your `PATH` (for bash, add this to your `~/.bashrc`):

    export PATH="$HOME/.cabal/bin:$PATH"

Log out and in again, and verify that your PATH is set up correctly:

    $ which shellcheck
    ~/.cabal/bin/shellcheck

On native Windows, the `PATH` should already be set up, but the system
may use a legacy codepage. In `cmd.exe`, `powershell.exe` and Powershell ISE,
make sure to use a TrueType font, not a Raster font, and set the active
codepage to UTF-8 (65001) with `chcp`:

    > chcp 65001
    Active code page: 65001

In Powershell ISE, you may need to additionally update the output encoding:

    > [Console]::OutputEncoding = [System.Text.Encoding]::UTF8

#### Running tests

To run the unit test suite:

    $ cabal test


## Gallery of bad code
So what kind of things does ShellCheck look for? Here is an incomplete list of detected issues.

#### Quoting

ShellCheck can recognize several types of incorrect quoting:

    echo $1                           # Unquoted variables
    find . -name *.ogg                # Unquoted find/grep patterns
    rm "~/my file.txt"                # Quoted tilde expansion
    v='--verbose="true"'; cmd $v      # Literal quotes in variables
    for f in "*.ogg"                  # Incorrectly quoted 'for' loops
    touch $@                          # Unquoted $@
    echo 'Don't forget to restart!'   # Singlequote closed by apostrophe
    echo 'Don\'t try this at home'    # Attempting to escape ' in ''
    echo 'Path is $PATH'              # Variables in single quotes
    trap "echo Took ${SECONDS}s" 0    # Prematurely expanded trap


#### Conditionals

ShellCheck can recognize many types of incorrect test statements.

    [[ n != 0 ]]                      # Constant test expressions
    [[ -e *.mpg ]]                    # Existence checks of globs
    [[ $foo==0 ]]                     # Always true due to missing spaces
    [[ -n "$foo " ]]                  # Always true due to literals
    [[ $foo =~ "fo+" ]]               # Quoted regex in =~
    [ foo =~ re ]                     # Unsupported [ ] operators
    [ $1 -eq "shellcheck" ]           # Numerical comparison of strings
    [ $n && $m ]                      # && in [ .. ]
    [ grep -q foo file ]              # Command without $(..)


#### Frequently misused commands

ShellCheck can recognize instances where commands are used incorrectly:

    grep '*foo*' file                 # Globs in regex contexts
    find . -exec foo {} && bar {} \;  # Prematurely terminated find -exec
    sudo echo 'Var=42' > /etc/profile # Redirecting sudo
    time --format=%s sleep 10         # Passing time(1) flags to time builtin
    while read h; do ssh "$h" uptime  # Commands eating while loop input
    alias archive='mv $1 /backup'     # Defining aliases with arguments
    tr -cd '[a-zA-Z0-9]'              # [] around ranges in tr
    exec foo; echo "Done!"            # Misused 'exec'
    find -name \*.bak -o -name \*~ -delete  # Implicit precedence in find
    f() { whoami; }; sudo f           # External use of internal functions


#### Common beginner's mistakes

ShellCheck recognizes many common beginner's syntax errors:

    var = 42                          # Spaces around = in assignments
    $foo=42                           # $ in assignments
    for $var in *; do ...             # $ in for loop variables
    var$n="Hello"                     # Wrong indirect assignment
    echo ${var$n}                     # Wrong indirect reference
    var=(1, 2, 3)                     # Comma separated arrays
    echo "Argument 10 is $10"         # Positional parameter misreference
    if $(myfunction); then ..; fi     # Wrapping commands in $()
    else if othercondition; then ..   # Using 'else if'


#### Style

ShellCheck can make suggestions to improve style:

    [[ -z $(find /tmp | grep mpg) ]]  # Use grep -q instead
    a >> log; b >> log; c >> log      # Use a redirection block instead
    echo "The time is `date`"         # Use $() instead
    cd dir; process *; cd ..;         # Use subshells instead
    echo $[1+2]                       # Use standard $((..)) instead of old $[]
    echo $(($RANDOM % 6))             # Don't use $ on variables in $((..))
    echo "$(date)"                    # Useless use of echo
    cat file | grep foo               # Useless use of cat


#### Data and typing errors

ShellCheck can recognize issues related to data and typing:

    args="$@"                         # Assigning arrays to strings
    files=(foo bar); echo "$files"    # Referencing arrays as strings
    printf "%s\n" "Arguments: $@."    # Concatenating strings and arrays.
    [[ $# > 2 ]]                      # Comparing numbers as strings
    var=World; echo "Hello " var      # Unused lowercase variables
    echo "Hello $name"                # Unassigned lowercase variables
    cmd | read bar; echo $bar         # Assignments in subshells


#### Robustness

ShellCheck can make suggestions for improving the robustness of a script:

    rm -rf "$STEAMROOT/"*            # Catastrophic rm
    touch ./-l; ls *                 # Globs that could become options
    find . -exec sh -c 'a && b {}' \; # Find -exec shell injection
    printf "Hello $name"             # Variables in printf format
    for f in $(ls *.txt); do         # Iterating over ls output
    export MYVAR=$(cmd)              # Masked exit codes


#### Portability

ShellCheck will warn when using features not supported by the shebang. For example, if you set the shebang to `#!/bin/sh`, ShellCheck will warn about portability issues similar to `checkbashisms`:


    echo {1..$n}                     # Works in ksh, but not bash/dash/sh
    echo {1..10}                     # Works in ksh and bash, but not dash/sh
    echo -n 42                       # Works in ksh, bash and dash, undefined in sh
    trap 'exit 42' sigint            # Unportable signal spec
    cmd &> file                      # Unportable redirection operator
    read foo < /dev/tcp/host/22      # Unportable intercepted files
    foo-bar() { ..; }                # Undefined/unsupported function name
    [ $UID = 0 ]                     # Variable undefined in dash/sh
    local var=value                  # local is undefined in sh


#### Miscellaneous

ShellCheck recognizes a menagerie of other issues:

    PS1='\e[0;32m\$\e[0m '            # PS1 colors not in \[..\]
    PATH="$PATH:~/bin"                # Literal tilde in $PATH
    rm “file”                         # Unicode quotes
    echo "Hello world"                # Carriage return / DOS line endings
    var=42 echo $var                  # Expansion of inlined environment
    #!/bin/bash -x -e                 # Common shebang errors
    echo $((n/180*100))               # Unnecessary loss of precision
    ls *[:digit:].txt                 # Bad character class globs
    sed 's/foo/bar/' file > file       # Redirecting to input


## Testimonials

> At first you're like "shellcheck is awesome" but then you're like "wtf are we still using bash"

Alexander Tarasikov,
[via Twitter](https://twitter.com/astarasikov/status/568825996532707330)


## Reporting bugs

Please use the GitHub issue tracker for any bugs or feature suggestions:

https://github.com/koalaman/shellcheck/issues


## Contributing

Please submit patches to code or documentation as GitHub pull requests!

Contributions must be licensed under the GNU GPLv3.
The contributor retains the copyright.


## Copyright

ShellCheck is licensed under the GNU General Public License, v3. A copy of this license is included in the file [LICENSE](LICENSE).

Copyright 2012-2015, Vidar 'koala_man' Holen and contributors.

Happy ShellChecking!
