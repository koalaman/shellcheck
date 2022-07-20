# I've again spent days trying to get a working armv6hf compiler going.
# God only knows how many recompilations of GCC, GHC, libraries, and
# ShellCheck itself, has gone into it.
#
# I tried Debian's toolchain.  I tried my custom one built according to
# RPi `gcc -v`. I tried GHC9, glibc, musl, registerised vs not, but
# nothing has yielded an armv6hf binary that does not immediately
# segfault on qemu-arm-static or the RPi itself.
#
# I then tried the same but with armv7hf. Same story.
#
# Emulating the entire userspace with balenalib again?  Very strange build
# failures where programs would fail to execute with > ~100 arguments.
#
# Finally, creating our own appears to work when using a custom QEmu
# patched to follow execve calls.
#
# PS: $100 bounty for getting a RPi1 compatible static build going
#     with cross-compilation, similar to what the aarch64 build does.
#

FROM ubuntu:20.04

ENV TARGETNAME linux.armv6hf

# Build QEmu with execve follow support
USER root
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y build-essential git ninja-build python3 pkg-config libglib2.0-dev libpixman-1-dev
WORKDIR /build
RUN git clone --depth 1 https://github.com/koalaman/qemu
RUN cd qemu && ./configure --static && cd build && ninja qemu-arm
RUN cp qemu/build/qemu-arm /build/qemu-arm-static
ENV QEMU_EXECVE 1

# Set up an armv6 userspace
WORKDIR /
RUN apt-get install -y debootstrap qemu-user-static
# We expect this to fail if the host doesn't have binfmt qemu support
RUN qemu-debootstrap --arch armhf bullseye pi http://mirrordirector.raspbian.org/raspbian || [ -e /pi/etc/issue ]
RUN cp /build/qemu-arm-static /pi/usr/bin/qemu-arm-static
RUN printf > /bin/pirun '%s\n' '#!/bin/sh' 'chroot /pi /usr/bin/qemu-arm-static /usr/bin/env "$@"' && chmod +x /bin/pirun
# If the debootstrap process didn't finish, continue it
RUN [ ! -e /pi/debootstrap ] || pirun '/debootstrap/debootstrap' --second-stage

# Install deps in the chroot
RUN pirun apt-get update
RUN pirun apt-get install -y ghc cabal-install

# Finally we can build the current dependencies. This takes hours.
ENV CABALOPTS "--ghc-options;-split-sections -optc-Os -optc-Wl,--gc-sections;--gcc-options;-Os -Wl,--gc-sections -ffunction-sections -fdata-sections"
RUN pirun cabal update
RUN IFS=";" && pirun cabal install --dependencies-only $CABALOPTS ShellCheck
RUN IFS=';' && pirun cabal install $CABALOPTS --lib fgl

# Copy the build script
WORKDIR /pi/scratch
COPY build /pi/usr/bin
ENTRYPOINT ["/bin/pirun", "/usr/bin/build"]
