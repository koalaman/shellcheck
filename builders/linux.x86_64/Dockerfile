FROM alpine:3.22
# alpine:3.16 (GHC 9.0.1):  5.8 megabytes (certs expired)
# alpine:3.17 (GHC 9.0.2): 15.0 megabytes (certs expired)
# alpine:3.18 (GHC 9.4.4): 29.0 megabytes (certs expired)
# alpine:3.19 (GHC 9.4.7): 29.0 megabytes (certs expired)
# alpine:3.20 (GHC 9.8.2): 16.0 megabytes
# alpine:3.21 (GHC 9.8.2): 16.0 megabytes
# alpine:3.22 (GHC 9.8.2): 16.0 megabytes

ENV TARGETNAME=linux.x86_64

# Install GHC and cabal
USER root
RUN apk add ghc cabal g++ libffi-dev curl bash gmp gmp-static

# Cabal has failed to cache if options are not specified on the command line,
# so do that explicitly.
ENV CABALOPTS="--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections"

# Verify that we have the certificates in place to successfully update cabal
RUN cabal update && rm -rf ~/.cabal

# Other archs pre-build dependencies here, but this one doesn't to detect ecosystem movement
RUN true

# Copy the build script
COPY build /usr/bin

WORKDIR /scratch
ENTRYPOINT ["/usr/bin/build"]
