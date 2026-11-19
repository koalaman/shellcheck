FROM ubuntu:26.04

ENV TARGET=arm-linux-gnueabihf
ENV TARGETNAME=linux.armv6hf

# Build dependencies
# GHC 9.14 supports LLVM < 21, and GNU ld is affected by binutils #16177 on ARM
# (checked by GHC's configure), so use clang/lld 20 as the toolchain.
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y llvm-20 clang-20 lld-20 binutils-arm-linux-gnueabihf ghc alex happy automake autoconf build-essential curl xz-utils qemu-user mmdebstrap
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-linux-alpine3_20.tar.xz" | tar xJv -C /usr/local/bin && cabal update

# Ubuntu's armhf toolchain and libraries are ARMv7, while Raspbian's are ARMv6.
# Use Raspbian's libraries as a sysroot, with clang targeting the ARM1176 (as
# GHC's LLVM backend does for arm-unknown-linux-gnueabihf).
# base-files provides the /lib symlink that glibc's libc.so linker script needs.
# The key is from https://archive.raspbian.org/raspbian.public.key
COPY raspbian.public.asc /etc/
RUN mmdebstrap --variant=extract --architectures=armhf --keyring=/etc/raspbian.public.asc \
      --include=base-files,libc6-dev,libgcc-14-dev,libstdc++-14-dev,libffi-dev,libcrypt-dev \
      trixie /sysroot http://mirrordirector.raspbian.org/raspbian
COPY wrapper /usr/local/bin/
RUN for t in gcc cc g++ c++ clang clang++ llc; do ln -s wrapper "/usr/local/bin/$TARGET-$t"; done

# GHC fails to build if it can't encode non-ascii
ENV LC_CTYPE=C.utf8

# Build GHC. This is a single step so the build tree doesn't end up in the image.
# - Profiled libraries are disabled because llc-20 segfaults on them.
# - The bindist's configure doesn't know armv6 and would pick the host
#   toolchain, so the triples and tools are specified explicitly.
ENV GHCTOOLS="LLC=$TARGET-llc OPT=opt-20 LLVMAS=$TARGET-clang LD=/usr/lib/llvm-20/bin/ld.lld"
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.14.1/ghc-9.14.1-src.tar.xz" | tar xJ --strip-components=1 && \
    ./boot.source && \
    env $GHCTOOLS ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET" --disable-ld-override && \
    ./hadrian/build --flavour=perf+split_sections+no_dynamic_libs+no_profiled_libs --bignum=native --docs=none -V -j binary-dist-dir && \
    cd _build/bindist/ghc-*-"$TARGET" && \
    env $GHCTOOLS CC="$TARGET-gcc" CXX="$TARGET-g++" AR="$TARGET-ar" NM="$TARGET-nm" RANLIB="$TARGET-ranlib" \
      OBJDUMP="$TARGET-objdump" OBJCOPY="$TARGET-objcopy" READELF="$TARGET-readelf" STRIP="$TARGET-strip" \
      sh configure --prefix=/usr/local --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target="$TARGET" --disable-ld-override && \
    make install && cd / && rm -rf /ghc

# Prebuild the dependencies of the latest release
COPY cabal.project /etc/
RUN cd /tmp && cabal update && cabal get ShellCheck && cd ShellCheck-*/ && \
    cp /etc/cabal.project . && cabal build --only-dependencies && cd .. && rm -rf ShellCheck-*/

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
