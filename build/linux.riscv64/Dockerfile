FROM ubuntu:24.04

ENV TARGETNAME linux.riscv64
ENV TARGET riscv64-linux-gnu

USER root
ENV DEBIAN_FRONTEND noninteractive

# Init base
RUN apt-get update -y

# Install qemu
RUN apt-get install -y --no-install-recommends build-essential ninja-build python3 pkg-config libglib2.0-dev libpixman-1-dev curl ca-certificates python3-virtualenv git python3-setuptools
WORKDIR /qemu
RUN git clone --depth 1 https://github.com/koalaman/qemu .
#RUN git clone https://github.com/balena-io/qemu .
# Release 7.0.0
#RUN git checkout 639d1d8903f65d74eb04c49e0df7a4b2f014cd86
RUN ./configure --target-list=riscv64-linux-user --static --disable-system --disable-pie --disable-werror
RUN cd build && ninja qemu-riscv64
ENV QEMU_EXECVE 1


# Set up a riscv64 userspace
RUN apt-get install -y --no-install-recommends debootstrap
RUN debootstrap --arch=riscv64 --foreign noble /rvfs http://ports.ubuntu.com/ubuntu-ports
RUN cp /qemu/build/qemu-riscv64 /rvfs/usr/bin/qemu-riscv64-static

# Command to run riscv binaries in the chroot. The Haskell runtime allocates 1TB
# vspace up front and QEmu has a RAM cost per vspace, so use ulimit to allocate
# less and reduce RAM usage.
RUN printf > /bin/rv '%s\n' '#!/bin/sh' 'ulimit -v $((10*1024*1024)); chroot /rvfs /usr/bin/qemu-riscv64-static /usr/bin/env "$@"'
RUN chmod +x /bin/rv
RUN [ ! -e /rvfs/debootstrap ] || rv '/debootstrap/debootstrap' --second-stage

# Install deps in the chroot
RUN printf > /rvfs/etc/apt/sources.list '%s\n' 'deb http://ports.ubuntu.com/ubuntu-ports noble main universe'
RUN rv apt-get update -y
RUN rv apt-get install -y --no-install-recommends ghc cabal-install

RUN rv cabal update
# Generated with: cabal freeze -c 'hashable -arch-native'. We put it in /etc so cabal won't find it.
COPY cabal.project.freeze /rvfs/etc
# Awful hack to install everything from the freeze file
# This basically turns 'any.tagged ==0.8.8' into tagged-0.8.8 to install by version,
# and adds a -c before 'hashable -arch-native +integer-gmp' to make it a flag constraint.
RUN < /rvfs/etc/cabal.project.freeze sed 's/constraints:/&\n /' | grep -vw rts | sed -n -e 's/^  *\([^,]*\).*/\1/p' | sed -e 's/any\.\([^ ]*\) ==\(.*\)/\1-\2/; te; s/.*/-c\n&/; :e' > /tmp/preinstall-flags
# Finally we can build the current dependencies. This takes hours.
# There's apparently a random segfault during assembly, so retry a few times.
RUN set -x; IFS=${IFS# }; f() { rv cabal install --keep-going $(cat /tmp/preinstall-flags); }; for i in $(seq 5); do f; ret=$?; [ $ret = 0 ] && break; done; exit $ret

# Copy the build script
WORKDIR /rvfs/scratch
COPY build /rvfs/usr/bin/build
ENTRYPOINT ["/bin/rv", "/usr/bin/build"]
