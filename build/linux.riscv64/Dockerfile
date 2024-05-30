FROM ubuntu:22.04

ENV TARGETNAME linux.riscv64
ENV TARGET riscv64-linux-gnu

USER root
ENV DEBIAN_FRONTEND noninteractive

# Init base
RUN apt update -y

# Install qemu
RUN apt install -y --no-install-recommends build-essential ninja-build python3 pkg-config libglib2.0-dev libpixman-1-dev curl ca-certificates python3-virtualenv
WORKDIR /qemu
RUN curl -Lv "https://download.qemu.org/qemu-9.0.0.tar.xz" | tar xJ --strip-components=1
RUN ./configure --target-list=riscv64-linux-user --static --disable-system --disable-pie
RUN cd build && ninja qemu-riscv64
ENV QEMU_EXECVE 1


# Set up a riscv64 userspace
RUN apt install -y --no-install-recommends debootstrap
RUN debootstrap --arch=riscv64 --foreign jammy /rvfs http://ports.ubuntu.com/ubuntu-ports
RUN cp /qemu/build/qemu-riscv64 /rvfs/usr/bin/qemu-riscv64-static

RUN printf > /bin/rv '%s\n' '#!/bin/sh' 'chroot /rvfs /usr/bin/qemu-riscv64-static /usr/bin/env "$@"'
RUN chmod +x /bin/rv
RUN [ ! -e /rvfs/debootstrap ] || rv '/debootstrap/debootstrap' --second-stage

# Install deps in the chroot
RUN printf > /rvfs/etc/apt/sources.list '%s\n' 'deb http://ports.ubuntu.com/ubuntu-ports jammy main universe'
RUN rv apt update -y
RUN rv apt install -y --no-install-recommends ghc cabal-install

# Finally we can build the current dependencies. This takes hours.
# jobs must be 1, GHS riscv will use about 40G memory
RUN rv cabal update
RUN IFS=";" && rv cabal install --dependencies-only --jobs=1 ShellCheck
RUN IFS=';' && rv cabal install --lib --jobs=1 fgl

# Clean up
RUN rm -rf /qemu

# Copy the build script
WORKDIR /rvfs/scratch
COPY build /rvfs/usr/bin/build
ENTRYPOINT ["/bin/rv", "/usr/bin/build"]
