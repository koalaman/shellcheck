# Build-only image
FROM ubuntu:17.10 AS build
USER root
WORKDIR /opt/shellCheck

# Install OS deps
RUN apt-get update && apt-get install -y ghc cabal-install

# Install Haskell deps
# (This is a separate copy/run so that source changes don't require rebuilding)
COPY ShellCheck.cabal ./
RUN cabal update && cabal install --dependencies-only

# Copy source and build it
COPY LICENSE Setup.hs shellcheck.hs ./
COPY src src
RUN cabal build Paths_ShellCheck && \
  ghc -optl-static -optl-pthread -isrc -idist/build/autogen --make shellcheck && \
  strip --strip-all shellcheck

RUN mkdir -p /out/bin && \
  cp shellcheck  /out/bin/

# Resulting Alpine image
FROM alpine:latest
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
COPY --from=build /out /
WORKDIR /mnt
ENTRYPOINT ["/bin/shellcheck"]

# DELETE-MARKER (Remove everything below to keep the alpine image)

# Resulting ShellCheck image
FROM scratch
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
COPY --from=build /out /
WORKDIR /mnt
ENTRYPOINT ["/bin/shellcheck"]
