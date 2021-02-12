# DIGEST:sha256:fa32af4677e2860a1c5950bc8c360f309e2a87e2ddfed27b642fddf7a6093b76
FROM liushuyu/osxcross:latest

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
RUN cabal update && IFS=';' && cabal install $CABALOPTS --lib Diff-0.4.0 base-compat-0.11.2 base-orphans-0.8.4 dlist-1.0 hashable-1.3.0.0 indexed-traversable-0.1.1 integer-logarithms-1.0.3.1 primitive-0.7.1.0 regex-base-0.94.0.0 splitmix-0.1.0.3 tagged-0.8.6.1 th-abstraction-0.4.2.0 transformers-compat-0.6.6 base-compat-batteries-0.11.2 time-compat-1.9.5 unordered-containers-0.2.13.0 data-fix-0.3.1 vector-0.12.2.0 scientific-0.3.6.2 regex-tdfa-1.3.1.0 random-1.2.0 distributive-0.6.2.1 attoparsec-0.13.2.5 uuid-types-1.0.3 comonad-5.0.8 bifunctors-5.5.10 assoc-1.0.2 these-1.1.1.1 strict-0.4.0.1 aeson-1.5.5.1

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
