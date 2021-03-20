FROM ubuntu:20.04

ENV TARGETNAME windows.x86_64

# We don't need wine32, even though it complains
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y curl busybox wine winbind

# Fetch Windows version, will be available under z:\haskell
WORKDIR /haskell
RUN curl -L "https://downloads.haskell.org/~ghc/8.10.4/ghc-8.10.4-x86_64-unknown-mingw32.tar.xz" | tar xJ --strip-components=1
WORKDIR /haskell/bin
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip" | busybox unzip -
RUN curl -L "https://curl.se/windows/dl-7.75.0/curl-7.75.0-win64-mingw.zip" | busybox unzip - && mv curl-7.75.0-win64-mingw/bin/* .
ENV WINEPATH /haskell/bin

# It's unknown whether Cabal on Windows suffers from the same issue
# that necessitated this but I don't care enough to find out
ENV CABALOPTS "--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections"

# Precompile some deps to speed up later builds
RUN wine /haskell/bin/cabal.exe update && IFS=';' && wine /haskell/bin/cabal.exe install --lib --dependencies-only $CABALOPTS ShellCheck

COPY build /usr/bin
WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
