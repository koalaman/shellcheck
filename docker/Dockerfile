FROM ubuntu:16.04
MAINTAINER koalaman <koalaman@gmail.com>

RUN apt-get update -y && \
    apt-get -y --no-install-recommends install git cabal-install && \
    apt-get clean && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/*

RUN git config --global http.sslVerify false
WORKDIR /root
RUN git clone https://github.com/koalaman/shellcheck.git
WORKDIR /root/shellcheck
RUN cabal update && cabal install

ENTRYPOINT ["/root/.cabal/bin/shellcheck"]
