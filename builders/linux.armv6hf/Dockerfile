# This Docker file uses a custom QEmu fork with patches to follow execve
# to build all of ShellCheck emulated.

FROM ubuntu:24.04

ENV TARGETNAME linux.armv6hf

# Build QEmu with execve follow support
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y --no-install-recommends build-essential git ninja-build python3 pkg-config libglib2.0-dev libpixman-1-dev python3-setuptools ca-certificates debootstrap
WORKDIR /qemu
RUN git clone --depth 1 https://github.com/koalaman/qemu .
RUN ./configure --static --disable-werror && cd build && ninja qemu-arm
ENV QEMU_EXECVE 1

# Convenience utility
COPY scutil /bin/scutil
COPY scutil /chroot/bin/scutil
RUN chmod +x /bin/scutil /chroot/bin/scutil

# Set up an armv6 userspace
WORKDIR /
RUN debootstrap --arch armhf --variant=minbase --foreign bookworm /chroot http://mirrordirector.raspbian.org/raspbian
RUN cp /qemu/build/qemu-arm /chroot/bin/qemu
RUN scutil emu /debootstrap/debootstrap --second-stage

# Install deps in the chroot
RUN scutil emu apt-get update
RUN scutil emu apt-get install -y --no-install-recommends ghc cabal-install
RUN scutil emu cabal update

# Finally we can build the current dependencies. This takes hours.
ENV CABALOPTS "--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections;--gcc-options;-Os -Wl,--gc-sections -ffunction-sections -fdata-sections"
# Generated with `cabal freeze --constraint 'hashable -arch-native'`
COPY cabal.project.freeze /chroot/etc
RUN IFS=";" && scutil install_from_freeze /chroot/etc/cabal.project.freeze emu cabal install $CABALOPTS

# Copy the build script
COPY build /chroot/bin
ENTRYPOINT ["/bin/scutil", "emu", "/bin/build"]
