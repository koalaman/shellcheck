# osxcross's tools are built for Ubuntu 24.04 (they need libxml2.so.2)
FROM ubuntu:24.04

# GHC's target has no OS version, since Hadrian mangles the names of
# executables when the target has a dot in it. osxcross's tools are named
# after the SDK version, so they're passed to GHC explicitly.
ENV TARGET=aarch64-apple-darwin
ENV OSXCROSS=arm64-apple-darwin25.1
ENV TARGETNAME=darwin.aarch64

# Build dependencies
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y clang libxml2 libgmp-dev alex happy automake autoconf build-essential curl xz-utils file
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-linux-alpine3_20.tar.xz" | tar xJv -C /usr/local/bin && cabal update

# macOS cross toolchain and SDK, copied from crazymax/osxcross:latest-ubuntu
COPY --from=koalaman/scbuilder-osxcross /osxcross /osxcross
ENV PATH=/osxcross/bin:$PATH
ENV LD_LIBRARY_PATH=/osxcross/lib

# GHC passes its LLVM target (arm64-apple-darwin) to clang as --target,
# and clang then looks for a linker by that name. Without it, clang would
# fall back to the host's /usr/bin/ld.
RUN ln -s "/osxcross/bin/$OSXCROSS-ld" "/usr/local/bin/${OSXCROSS%%-*}-apple-darwin-ld"

# Building GHC 9.14 needs GHC 9.6 to 9.12, and Ubuntu 24.04 has 9.4
WORKDIR /host-ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.12.2/ghc-9.12.2-x86_64-ubuntu22_04-linux.tar.xz" | tar xJ --strip-components=1 && \
    ./configure --prefix=/usr/local && make install && cd / && rm -rf /host-ghc

# GHC fails to build if it can't encode non-ascii
ENV LC_CTYPE=C.utf8

# Build GHC. This is a single step so the build tree doesn't end up in the image.
# - Cabal passes --host=<arch>-osx to the configure scripts of GHC's own
#   packages (like rts), so teach GHC's OS name conversion about "osx".
# - We pass split_sections but "Dead code elimination is broken on AArch64/Darwin" (GHC #24962)
ENV GHCTOOLS="CC=$OSXCROSS-clang CXX=$OSXCROSS-clang++ LD=$OSXCROSS-ld AR=$OSXCROSS-ar NM=$OSXCROSS-nm RANLIB=$OSXCROSS-ranlib STRIP=$OSXCROSS-strip OTOOL=$OSXCROSS-otool INSTALL_NAME_TOOL=$OSXCROSS-install_name_tool"
WORKDIR /ghc
RUN curl -L "https://downloads.haskell.org/~ghc/9.14.1/ghc-9.14.1-src.tar.xz" | tar xJ --strip-components=1 && \
    sed -i 's/^      darwin\*) # e.g. aarch64-apple-darwin14/      osx|darwin*) # e.g. aarch64-apple-darwin14/' m4/ghc_convert_os.m4 && \
    grep -q "osx|darwin" m4/ghc_convert_os.m4 && \
    ./boot.source && \
    env $GHCTOOLS ./configure --host x86_64-linux-gnu --build x86_64-linux-gnu --target "$TARGET" && \
    ./hadrian/build --flavour=perf+split_sections+no_dynamic_libs --bignum=native --docs=none -j binary-dist-dir && \
    cd _build/bindist/ghc-*-"$TARGET" && \
    env $GHCTOOLS sh configure --prefix=/usr/local --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target="$TARGET" && \
    make install && cd / && rm -rf /ghc

# Prebuild the dependencies of the latest release
COPY cabal.project /etc/
RUN cd /tmp && cabal get ShellCheck && cd ShellCheck-*/ && \
    cp /etc/cabal.project . && cabal build --only-dependencies && cd .. && rm -rf ShellCheck-*/

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
