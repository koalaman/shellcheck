FROM ghcr.io/shepherdjerred/macos-cross-compiler:latest

ENV TARGET aarch64-apple-darwin22
ENV TARGETNAME darwin.aarch64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND noninteractive
ENV LC_ALL C.utf8

# Install basic deps
RUN apt-get update && apt-get install -y automake autoconf build-essential curl xz-utils qemu-user-static

# Install a more suitable host compiler
WORKDIR /host-ghc
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.9.0.0/cabal-install-3.9-x86_64-linux-alpine.tar.xz" | tar xJv -C /usr/local/bin
RUN curl -L 'https://downloads.haskell.org/~ghc/8.10.7/ghc-8.10.7-x86_64-deb10-linux.tar.xz' | tar xJ --strip-components=1
RUN ./configure && make install

# Build GHC. We have to use an old version because cross-compilation across OS has since broken.
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/8.10.7/ghc-8.10.7-src.tar.xz" | tar xJ --strip-components=1
RUN apt-get install -y llvm-12
RUN ./boot && ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET"
RUN cp mk/flavours/quick-cross.mk mk/build.mk && make -j "$(nproc)"
RUN make install

# Due to an apparent cabal bug, we specify our options directly to cabal
# It won't reuse caches if ghc-options are specified in ~/.cabal/config
ENV CABALOPTS "--ghc-options;-optc-Os -optc-fPIC;--with-ghc=$TARGET-ghc;--with-hc-pkg=$TARGET-ghc-pkg;--constraint=hashable==1.3.5.0"

# Prebuild the dependencies
RUN cabal update
RUN IFS=';' && cabal install --dependencies-only $CABALOPTS ShellCheck

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
