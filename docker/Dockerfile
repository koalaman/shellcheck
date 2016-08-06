FROM ubuntu:16.04
MAINTAINER koalaman <koalaman@gmail.com>

RUN apt-get update -qy

RUN apt-get -qy install git cabal-install

RUN cd $HOME && git clone https://github.com/koalaman/shellcheck.git
RUN cd $HOME/shellcheck && cabal update
RUN cd $HOME/shellcheck && cabal install


# Clean apt cache
RUN apt-get clean && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/* \
    rm -rf /var/cache/*.tar.gz \
    rm -rf /var/cache/*/*.tar.gz

ENTRYPOINT ["/root/.cabal/bin/shellcheck"]
