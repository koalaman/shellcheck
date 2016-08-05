FROM ubuntu:xenial
MAINTAINER https://github.com/koalaman/shellcheck

RUN apt-get update && apt-get install --no-install-recommends -y \
    cabal-install \
    ghc \
 && rm -rf /var/lib/apt/lists/*

COPY ShellCheck.cabal /src/ShellCheck.cabal

WORKDIR /src

ENV PATH="/root/.cabal/bin:$PATH"

RUN cabal update \
 && cabal install --only-dependencies

COPY . /src

RUN cabal install /src

CMD ["shellcheck", "-"]
