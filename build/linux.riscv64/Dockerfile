FROM ubuntu:24.04

ENV TARGETNAME linux.riscv64
ENV TARGET riscv64-linux-gnu

USER root
ENV DEBIAN_FRONTEND noninteractive

# Init base
RUN apt-get update -y

# Install qemu
RUN apt-get install -y --no-install-recommends build-essential ninja-build python3 pkg-config libglib2.0-dev libpixman-1-dev curl ca-certificates python3-virtualenv git python3-setuptools debootstrap
WORKDIR /qemu
RUN git clone --depth 1 https://github.com/koalaman/qemu .
RUN ./configure --target-list=riscv64-linux-user --static --disable-system --disable-pie --disable-werror
RUN cd build && ninja qemu-riscv64
ENV QEMU_EXECVE 1

# Convenience utility
COPY scutil /bin/scutil
# We have to copy to /usr/bin because debootstrap will try to symlink /bin and fail if it exists
COPY scutil /chroot/usr/bin/scutil
RUN chmod +x /bin/scutil /chroot/usr/bin/scutil

# Set up a riscv64 userspace
WORKDIR /
RUN debootstrap --arch=riscv64 --variant=minbase --components=main,universe --foreign noble /chroot http://ports.ubuntu.com/ubuntu-ports
RUN cp /qemu/build/qemu-riscv64 /chroot/bin/qemu
RUN scutil emu /debootstrap/debootstrap --second-stage

# Install deps in the chroot
RUN scutil emu apt-get update
RUN scutil emu apt-get install -y --no-install-recommends ghc cabal-install
RUN scutil emu cabal update

# Generated with: cabal freeze -c 'hashable -arch-native'. We put it in /etc so cabal won't find it.
COPY cabal.project.freeze /chroot/etc

# Build all dependencies from the freeze file. The emulator segfaults at random,
# so retry a few times.
RUN scutil install_from_freeze /chroot/etc/cabal.project.freeze retry 5 emu cabal install --keep-going

# Copy the build script
COPY build /chroot/bin/build
ENTRYPOINT ["/bin/scutil", "emu", "/bin/build"]
