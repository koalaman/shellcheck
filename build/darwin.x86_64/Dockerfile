FROM liushuyu/osxcross@sha256:fa32af4677e2860a1c5950bc8c360f309e2a87e2ddfed27b642fddf7a6093b76

ENV TARGET x86_64-apple-darwin18
ENV TARGETNAME darwin.x86_64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN sed -e 's/focal/kinetic/g' -i /etc/apt/sources.list
RUN apt-get update
RUN apt-get dist-upgrade -y
RUN apt-get install -y ghc automake autoconf llvm curl alex happy

# Build GHC
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.2.5/ghc-9.2.5-src.tar.xz" | tar xJ --strip-components=1
RUN ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET"
RUN cp mk/flavours/quick-cross.mk mk/build.mk && make -j "$(nproc)"
RUN make install
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.9.0.0/cabal-install-3.9-x86_64-linux-alpine.tar.xz" | tar xJv -C /usr/local/bin

# Due to an apparent cabal bug, we specify our options directly to cabal
# It won't reuse caches if ghc-options are specified in ~/.cabal/config
ENV CABALOPTS "--with-ghc=$TARGET-ghc;--with-hc-pkg=$TARGET-ghc-pkg"

# Prebuild the dependencies
RUN cabal update && IFS=';' && cabal install --dependencies-only $CABALOPTS ShellCheck

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
