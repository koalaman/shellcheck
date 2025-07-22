FROM ubuntu:25.04

ENV TARGET=aarch64-linux-gnu
ENV TARGETNAME=linux.aarch64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y llvm-20 "gcc-$TARGET" "g++-$TARGET" ghc alex happy automake autoconf build-essential curl qemu-user-static
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-linux-alpine3_20.tar.xz" | tar xJv -C /usr/local/bin && cabal update

# Build GHC
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.12.2/ghc-9.12.2-src.tar.xz" | tar xJ --strip-components=1
RUN ./boot.source && ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET"
# GHC fails to build if it can't encode non-ascii
ENV LC_CTYPE=C.utf8
# We have to do a binary-dist instead of a direct install, otherwise the targest won't have
# cross compilation prefixes in /usr/local/lib/aarch64-linux-gnu-ghc-*/lib/settings
RUN ./hadrian/build --flavour=quickest --bignum=native -V -j --prefix=/usr/local install
# Hadrian just outputs "gcc" as the name of gcc, without accounting for $TARGET. Manually fix up the paths:
RUN sed -e 's/"\(gcc\|g++\|ld\)"/"'"$TARGET"'-\1"/g' -i /usr/local/lib/$TARGET-ghc-*/lib/settings

# Due to an apparent cabal bug, we specify our options directly to cabal
# It won't reuse caches if ghc-options are specified in ~/.cabal/config
ENV CABALOPTS="--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections -optc-fPIC;--with-compiler=$TARGET-ghc;--with-hc-pkg=$TARGET-ghc-pkg;-c;hashable -arch-native"

# Prebuild the dependencies
RUN cabal update && IFS=';' && cabal install --dependencies-only $CABALOPTS ShellCheck

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
