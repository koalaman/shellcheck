# ShellCheck - A shell script static analysis tool

http://www.shellcheck.net

Copyright 2012-2015, Vidar 'koala_man' Holen
Licensed under the GNU General Public License, v3

The goals of ShellCheck are:

  - To point out and clarify typical beginner's syntax issues,
    that causes a shell to give cryptic error messages.

  - To point out and clarify typical intermediate level semantic problems,
    that causes a shell to behave strangely and counter-intuitively.

  - To point out subtle caveats, corner cases and pitfalls, that may cause an
    advanced user's otherwise working script to fail under future circumstances.

ShellCheck is written in Haskell, and requires 2 GB of memory to compile.

## Installing

On systems with Cabal:

    cabal update
    cabal install shellcheck

On Debian based distros:

    apt-get install shellcheck

On Fedora based distros:

    dnf install ShellCheck

On OS X with homebrew:

    brew install shellcheck

On openSUSE:Tumbleweed:

    zypper in ShellCheck

On other openSUSE distributions:

    add OBS devel:languages:haskell repository from (https://build.opensuse.org/project/repositories/devel:languages:haskell)
    zypper in ShellCheck

    or use OneClickInstall (https://software.opensuse.org/package/ShellCheck)

ShellCheck is also available as an online service:

    http://www.shellcheck.net

## Building with Cabal

This sections describes how to build ShellCheck from a source directory.

First, make sure cabal is installed. On Debian based distros:

    apt-get install cabal-install

On Fedora:

    yum install cabal-install

On openSUSE:Tumbleweed:

    zypper in cabal-install

On Mac OS X with homebrew (http://brew.sh/):

    brew install cabal-install

On Mac OS X with MacPorts (http://www.macports.org/):

    port install hs-cabal-install

On native Windows (https://www.haskell.org/platform/):

    Download and install the latest version of the Haskell Platform.

Let cabal update itself, in case your distro version is outdated:

    $ cabal update
    $ cabal install cabal-install

With cabal installed, cd to the ShellCheck source directory and:

    $ cabal install

This will install ShellCheck to your `~/.cabal/bin` directory.

Add the directory to your `PATH` (for bash, add this to your `~/.bashrc`):

    export PATH="$HOME/.cabal/bin:$PATH"

Verify that your PATH is set up correctly:

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

## Running tests

To run the unit test suite:

    cabal configure --enable-tests
    cabal build
    cabal test

## Reporting bugs

Please use the Github issue tracker for any bugs or feature suggestions:

https://github.com/koalaman/shellcheck/issues

Happy ShellChecking!
