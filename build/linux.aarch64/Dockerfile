FROM ubuntu:20.04

ENV TARGET aarch64-linux-gnu
ENV TARGETNAME linux.aarch64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND noninteractive

# These deps are from 20.04, because GHC's compiler/llvm support moves slowly
RUN apt-get update && apt-get install -y llvm gcc-$TARGET

# The rest are from 22.10
RUN sed -e 's/focal/kinetic/g' -i /etc/apt/sources.list
# Kinetic does not receive updates anymore, switch to last available
RUN sed -e 's/archive.ubuntu.com/old-releases.ubuntu.com/g' -i /etc/apt/sources.list
RUN sed -e 's/security.ubuntu.com/old-releases.ubuntu.com/g' -i /etc/apt/sources.list

RUN apt-get update && apt-get install -y ghc alex happy automake autoconf build-essential curl qemu-user-static

# Build GHC
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.2.8/ghc-9.2.8-src.tar.xz" | tar xJ --strip-components=1
RUN ./boot && ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET"
RUN cp mk/flavours/quick-cross.mk mk/build.mk && make -j "$(nproc)"
RUN make install
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.9.0.0/cabal-install-3.9-x86_64-linux-alpine.tar.xz" | tar xJv -C /usr/local/bin

# Due to an apparent cabal bug, we specify our options directly to cabal
# It won't reuse caches if ghc-options are specified in ~/.cabal/config
ENV CABALOPTS "--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections -optc-fPIC;--with-ghc=$TARGET-ghc;--with-hc-pkg=$TARGET-ghc-pkg;-c;hashable -arch-native"

# Prebuild the dependencies
RUN cabal update && IFS=';' && cabal install --dependencies-only $CABALOPTS ShellCheck

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
