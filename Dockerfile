# This file builds ShellCheck for pre-commit.
#
# It may also be useful for local development, but it is notably NOT
# used to build the official ShellCheck docker images.

FROM ubuntu:20.04
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y cabal-install
RUN cabal update

# Install dependencies separately for more efficient iteration
COPY ShellCheck.cabal /build/
RUN cd /build && cabal install --dependencies-only

# Now build the rest
COPY . /build/
RUN cd /build && cabal build shellcheck
RUN find /build -type f -name shellcheck -perm /111 -exec cp -f {} /bin \;
ENTRYPOINT ["/bin/shellcheck"]
