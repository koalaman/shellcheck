FROM ubuntu:26.04

ENV TARGETNAME=windows.x86_64

# We don't need wine32, even though it complains
USER root
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y curl busybox wine winbind xz-utils zstd

# Fetch Windows version, will be available under z:\haskell
WORKDIR /haskell
RUN curl -L "https://downloads.haskell.org/~ghc/9.14.1/ghc-9.14.1-x86_64-unknown-mingw32.tar.xz" | tar xJ --strip-components=1

# Fetch dependencies
WORKDIR /haskell/bin
RUN curl -L "https://downloads.haskell.org/~cabal/cabal-install-3.16.0.0/cabal-install-3.16.0.0-x86_64-windows.zip" | busybox unzip -
RUN curl -L "https://curl.se/windows/dl-8.23.0_9/curl-8.23.0_9-win64-mingw.zip" | busybox unzip - && mv curl-*-win64-mingw/bin/* .
RUN wine /haskell/bin/cabal.exe update
ENV WINEPATH=/haskell/bin:/haskell/mingw/bin

# Fetch GNU binutils from MSYS2, for linking and stripping.
# GHC's lld ignores split-sections on Windows.
WORKDIR /binutils
COPY binutils.sha256 .
RUN for f in $(cut -d' ' -f3 binutils.sha256); do curl -LO "https://repo.msys2.org/mingw/ucrt64/$f"; done && \
    sha256sum -c binutils.sha256 && \
    for f in *.pkg.tar.zst; do tar --zstd -xf "$f" ucrt64; done && rm *.pkg.tar.zst

# Prebuild the dependencies of the latest release
COPY cabal.project /etc/
RUN cd /tmp && wine /haskell/bin/cabal.exe get ShellCheck && cd ShellCheck-*/ && \
    cp /etc/cabal.project . && wine /haskell/bin/cabal.exe build --only-dependencies && cd .. && rm -rf ShellCheck-*/

COPY build /usr/bin
WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
