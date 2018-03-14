FROM ubuntu:16.04 AS build

# Install GHC and cabal
USER root
WORKDIR /opt/shellCheck

COPY . .

RUN apt-get update && apt-get install -y \
  ghc \
  cabal-install
RUN cabal update && \
  cabal install --dependencies-only
RUN cabal build Paths_ShellCheck && \
  ghc -optl-static -optl-pthread -idist/build/autogen --make shellcheck && \
  strip --strip-all shellcheck

RUN mkdir -p /out/bin && \
  cp shellcheck  /out/bin/

FROM scratch
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
WORKDIR /
COPY --from=build /out /
ENTRYPOINT ["/bin/shellcheck"]
