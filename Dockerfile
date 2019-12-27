# Build-only image
FROM ubuntu:18.04 AS build
USER root
WORKDIR /opt/shellCheck

# Install OS deps
RUN apt-get update && apt-get install -y ghc cabal-install

# Install Haskell deps
# (This is a separate copy/run so that source changes don't require rebuilding)
COPY ShellCheck.cabal ./
RUN cabal update && cabal install --dependencies-only --ghc-options="-optlo-Os -split-sections"

# Copy source and build it
COPY LICENSE shellcheck.hs ./
COPY src src
RUN cabal build Paths_ShellCheck && \
  ghc -optl-static -optl-pthread -isrc -idist/build/autogen --make shellcheck -split-sections -optc-Wl,--gc-sections -optlo-Os && \
  strip --strip-all shellcheck

RUN mkdir -p /out/bin && \
  cp shellcheck  /out/bin/

# Resulting ShellCheck image
FROM scratch
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
WORKDIR /mnt
COPY --from=build /out /
ENTRYPOINT ["/bin/shellcheck"]
