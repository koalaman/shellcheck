FROM ubuntu:20.04

ENV TARGETNAME windows.x86_64

# We don't need wine32, even though it complains
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y curl busybox wine

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

# Precompile some deps to speed up later builds. This list is just copied from `cabal build`
RUN wine /haskell/bin/cabal.exe update && IFS=';' && wine /haskell/bin/cabal.exe install $CABALOPTS --lib Diff-0.4.0 base-compat-0.11.2 base-orphans-0.8.4 dlist-1.0 hashable-1.3.0.0 indexed-traversable-0.1.1 integer-logarithms-1.0.3.1 primitive-0.7.1.0 regex-base-0.94.0.0 splitmix-0.1.0.3 tagged-0.8.6.1 th-abstraction-0.4.2.0 transformers-compat-0.6.6 base-compat-batteries-0.11.2 time-compat-1.9.5 unordered-containers-0.2.13.0 data-fix-0.3.1 vector-0.12.2.0 scientific-0.3.6.2 regex-tdfa-1.3.1.0 random-1.2.0 distributive-0.6.2.1 attoparsec-0.13.2.5 uuid-types-1.0.3 comonad-5.0.8 bifunctors-5.5.10 assoc-1.0.2 these-1.1.1.1 strict-0.4.0.1 aeson-1.5.5.1

COPY build /usr/bin
WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
