FROM alpine:3.16
# alpine:3.16 (GHC 9.0.1):  5.8 megabytes
# alpine:3.17 (GHC 9.0.2): 15.0 megabytes
# alpine:3.18 (GHC 9.4.4): 29.0 megabytes
# alpine:3.19 (GHC 9.4.7): 29.0 megabytes

ENV TARGETNAME linux.x86_64

# Install GHC and cabal
USER root
RUN apk add ghc cabal g++ libffi-dev curl bash

# Use ld.bfd instead of ld.gold due to
# x86_64-linux-gnu/libpthread.a(pthread_cond_init.o)(.note.stapsdt+0x14): error:
#    relocation refers to local symbol "" [2], which is defined in a discarded section
ENV CABALOPTS "--ghc-options;-optl-Wl,-fuse-ld=bfd -split-sections -optc-Os -optc-Wl,--gc-sections"

# Other archs pre-build dependencies here, but this one doesn't to detect ecosystem movement

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
