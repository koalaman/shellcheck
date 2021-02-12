FROM ubuntu:20.04

ENV TARGETNAME linux.x86_64

# Install GHC and cabal
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y ghc curl xz-utils

# So we'd like a later version of Cabal that supports --enable-executable-static,
# but we can't use Ubuntu 20.10 because coreutils has switched to new syscalls that
# the TravisCI kernel doesn't support. Download it manually.
RUN curl "https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz" | tar xJv -C /usr/bin

# Use ld.bfd instead of ld.gold due to
# x86_64-linux-gnu/libpthread.a(pthread_cond_init.o)(.note.stapsdt+0x14): error:
#    relocation refers to local symbol "" [2], which is defined in a discarded section
ENV CABALOPTS "--ghc-options;-optl-Wl,-fuse-ld=bfd -split-sections -optc-Os -optc-Wl,--gc-sections"

# Other archs pre-build dependencies here, but this one doesn't to detect ecosystem movement

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
