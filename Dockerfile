# Build-only image
FROM ubuntu:18.04 AS build
USER root
WORKDIR /opt/shellCheck

# Install OS deps, including GHC from HVR-PPA
# https://launchpad.net/~hvr/+archive/ubuntu/ghc
RUN apt-get -yq update \
  && apt-get -yq install software-properties-common \
  && apt-add-repository -y "ppa:hvr/ghc" \
  && apt-get -yq update \
  && apt-get -yq install cabal-install-2.4 ghc-8.4.3 pandoc \
  && rm -rf /var/lib/apt/lists/*

ENV PATH="/opt/ghc/bin:${PATH}"

# Use gold linker and check tools versions
RUN ln -s $(which ld.gold) /usr/local/bin/ld && \
     cabal --version \
  && ghc --version \
  && ld --version

# Install Haskell deps
# (This is a separate copy/run so that source changes don't require rebuilding)
#
# We also patch regex-tdfa and aeson removing hard-coded -O2 flag.
# This makes compilation faster and binary smaller.
# Performance loss is unnoticeable for ShellCheck
#
# Remember to update versions, once in a while.
COPY ShellCheck.cabal ./
RUN cabal update && \
  cabal get regex-tdfa-1.2.3.1 && sed -i 's/-O2//' regex-tdfa-1.2.3.1/regex-tdfa.cabal && \ 
  cabal get aeson-1.4.0.0 && sed -i 's/-O2//' aeson-1.4.0.0/aeson.cabal && \ 
  echo 'packages: . regex-tdfa-1.2.3.1 aeson-1.4.0.0 > cabal.project' && \
  cabal new-build --dependencies-only \
    --disable-executable-dynamic --enable-split-sections --disable-tests

# Copy source and build it
COPY LICENSE Setup.hs shellcheck.hs shellcheck.1.md ./
COPY src src
COPY test test
# This SED is the only "nastyness" we have to do
# Hopefully soon we could add per-component ld-options to cabal.project
RUN sed -i 's/-- STATIC/ld-options: -static -pthread -Wl,--gc-sections/' ShellCheck.cabal && \
  cat ShellCheck.cabal && \
  cabal new-build \
    --disable-executable-dynamic --enable-split-sections --disable-tests && \
  cp $(find dist-newstyle -type f -name shellcheck) . && \
  strip --strip-all shellcheck && \
  file shellcheck && \
  ls -l shellcheck

RUN mkdir -p /out/bin && \
  cp shellcheck  /out/bin/

# Resulting Alpine image
FROM alpine:latest
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
COPY --from=build /out /

# DELETE-MARKER (Remove everything below to keep the alpine image)

# Resulting ShellCheck image
FROM scratch
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
WORKDIR /mnt
COPY --from=build /out /
ENTRYPOINT ["/bin/shellcheck"]
