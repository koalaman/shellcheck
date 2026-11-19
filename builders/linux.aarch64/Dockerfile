FROM ubuntu:26.04

ENV TARGET=aarch64-linux-gnu
ENV TARGETNAME=linux.aarch64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y llvm "gcc-$TARGET" "g++-$TARGET" ghc alex happy automake autoconf build-essential curl qemu-user
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-linux-alpine3_20.tar.xz" | tar xJv -C /usr/local/bin && cabal update

# GHC fails to build if it can't encode non-ascii
ENV LC_CTYPE=C.utf8

# Build GHC. This is a single step so the build tree doesn't end up in the image.
# The binary distribution's configure is given the target's tools, which
# Hadrian's install target doesn't do.
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.14.1/ghc-9.14.1-src.tar.xz" | tar xJ --strip-components=1 && \
    ./boot.source && \
    ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET" && \
    ./hadrian/build --flavour=perf+split_sections+no_dynamic_libs --bignum=native --docs=none -V -j binary-dist-dir && \
    cd _build/bindist/ghc-*-"$TARGET" && \
    CC="$TARGET-gcc" CXX="$TARGET-g++" LD="$TARGET-ld" AR="$TARGET-ar" NM="$TARGET-nm" RANLIB="$TARGET-ranlib" \
      OBJDUMP="$TARGET-objdump" OBJCOPY="$TARGET-objcopy" READELF="$TARGET-readelf" STRIP="$TARGET-strip" \
      sh configure --prefix=/usr/local --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target="$TARGET" && \
    make install && cd / && rm -rf /ghc

# Prebuild the dependencies of the latest release
COPY cabal.project /etc/
RUN cd /tmp && cabal update && cabal get ShellCheck && cd ShellCheck-*/ && \
    cp /etc/cabal.project . && cabal build --only-dependencies && cd .. && rm -rf ShellCheck-*/

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
