# ShellCheck - A shell script static analysis tool

http://www.shellcheck.net

Copyright 2012-2014, Vidar 'koala_man' Holen
Licensed under the GNU Affero General Public License, v3

The goals of ShellCheck are:

  - To point out and clarify typical beginner's syntax issues,
    that causes a shell to give cryptic error messages.

  - To point out and clarify typical intermediate level semantic problems,
    that causes a shell to behave strangely and counter-intuitively.

  - To point out subtle caveats, corner cases and pitfalls, that may cause an
    advanced user's otherwise working script to fail under future circumstances.

ShellCheck is written in Haskell, and requires at least 1 GB of RAM to compile.

## Installing

Instead of building from source shellcheck can be installed through your
systems package manager.

On OS X:

    $ brew install shellscript

## Building with Cabal

Make sure cabal is installed. On Debian based distros:

    apt-get install cabal-install

On Fedora:

    yum install cabal-install

On Mac OS X with homebrew (http://brew.sh/):

    brew install cabal-install

On Mac OS X with MacPorts (http://www.macports.org/):

    port install hs-cabal-install

Let cabal update itself, in case your distro version is outdated:

    $ cabal update
    $ cabal install cabal-install

With cabal installed, cd to the ShellCheck source directory and:

    $ cabal install

This will install ShellCheck to your ~/.cabal/bin directory.

Add the directory to your PATH (for bash, add this to your ~/.bashrc file):

    export PATH=$HOME/.cabal/bin:$PATH

Verify that your PATH is set up correctly:

    $ which shellcheck
    ~/.cabal/bin/shellcheck

## Running tests

To run the unit test suite:

    cabal configure --enable-tests
    cabal build
    cabal test

## Building with Make

ShellCheck requires GHC, Parsec3, JSON and Text.Regex.

On Fedora, these can be installed with:

    yum install ghc ghc-parsec-devel ghc-QuickCheck-devel \
      ghc-json-devel ghc-regex-compat-devel pandoc

On Ubuntu and similar, use:

    apt-get install ghc libghc-parsec3-dev libghc-json-dev \
      libghc-regex-compat-dev libghc-quickcheck2-dev pandoc

To build, cd to the shellcheck source directory and:

    $ make

If you want to distribute the binary and/or run it on other distros, you
can `make shellcheck-static` to build a statically linked executable without
library dependencies.

Happy ShellChecking!
