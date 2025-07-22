FROM ubuntu:25.04

ENV TARGETNAME=windows.x86_64

# We don't need wine32, even though it complains
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y curl busybox wine winbind xz-utils

# Fetch Windows version, will be available under z:\haskell
WORKDIR /haskell
# 9.12.2 produces a 37M binary
# 9.0.2  produces a 28M binary
# 8.10.4 produces a 16M binary
# We don't want to be stuck on old versions forever though, so just go with the latest version
RUN curl -L "https://downloads.haskell.org/~ghc/9.12.2/ghc-9.12.2-x86_64-unknown-mingw32.tar.xz" | tar xJ --strip-components=1

# Fetch dependencies
WORKDIR /haskell/bin
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-windows.zip" | busybox unzip -
RUN curl -L "https://curl.se/windows/dl-8.15.0_2/curl-8.15.0_2-win64-mingw.zip" | busybox unzip - && mv curl-*-win64-mingw/bin/* .
RUN wine /haskell/bin/cabal.exe update
ENV WINEPATH=/haskell/bin:/haskell/mingw/bin

# None of these actually seem to have an effect on GHC on Windows anymore,
# but we'll leave them in place anyways.
ENV CABALOPTS="--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections"

# Precompile some deps to speed up later builds
RUN IFS=';' && wine /haskell/bin/cabal.exe install --lib --dependencies-only $CABALOPTS ShellCheck

COPY build /usr/bin
WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
