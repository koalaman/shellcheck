FROM ubuntu:16.04
MAINTAINER koalaman <koalaman@gmail.com>

RUN apt-get update -qqy

RUN apt-get -qqy install haskell-platform

RUN cabal update
RUN cabal install shellcheck

# Clean apt cache
RUN apt-get clean && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/* \
    rm -rf /var/cache/*.tar.gz \
    rm -rf /var/cache/*/*.tar.gz

ENTRYPOINT ["/root/.cabal/bin/shellcheck"]
