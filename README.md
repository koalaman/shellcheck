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

ShellCheck is written in Haskell, and requires GHC, Parsec3, JSON and
Text.Regex. To run the unit tests, it also requires QuickCheck2.

ShellCheck requires at least 1 GB of RAM to compile.

On Fedora, these can be installed with:

    yum install cabal-install ghc ghc-parsec-devel ghc-QuickCheck-devel \
      ghc-json-devel ghc-regex-compat-devel

On Ubuntu and similar, use:

    apt-get install ghc libghc-parsec3-dev libghc-json-dev \
      libghc-regex-compat-dev libghc-quickcheck2-dev cabal-install

For older releases, you may have to use:

    apt-get install ghc6 libghc6-parsec3-dev libghc6-quickcheck2-dev \
      libghc6-json-dev libghc-regex-compat-dev cabal-install

On Mac OS X with homebrew (http://brew.sh/), use:

    brew install cabal-install

On Mac OS X with MacPorts (http://www.macports.org/), use:

    port install hs-cabal-install

Executables can be built with cabal. Tests currently still rely on a Makefile.

Install:

    $ cabal install
    ...
    $ which shellcheck
    ~/.cabal/bin/shellcheck

Happy ShellChecking!
