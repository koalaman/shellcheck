FROM liushuyu/osxcross@sha256:fa32af4677e2860a1c5950bc8c360f309e2a87e2ddfed27b642fddf7a6093b76

ENV TARGET x86_64-apple-darwin18
ENV TARGETNAME darwin.x86_64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y ghc automake autoconf llvm curl

# Build GHC
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/8.10.4/ghc-8.10.4-src.tar.xz" | tar xJ --strip-components=1
RUN ./boot && ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET"
RUN cp mk/flavours/quick-cross.mk mk/build.mk && make -j "$(nproc)"
RUN make install
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz" | tar xJv -C /usr/local/bin

# Due to an apparent cabal bug, we specify our options directly to cabal
# It won't reuse caches if ghc-options are specified in ~/.cabal/config
ENV CABALOPTS "--with-ghc=$TARGET-ghc;--with-hc-pkg=$TARGET-ghc-pkg"

# Prebuild the dependencies
RUN cabal update && IFS=';' && cabal install --dependencies-only $CABALOPTS ShellCheck

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
