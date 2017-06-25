FROM scratch

MAINTAINER Vidar Holen <vidar@vidarholen.net>

# This file assumes ShellCheck has already been built.
# See https://github.com/koalaman/scbuilder
COPY shellcheck /

WORKDIR /mnt
ENTRYPOINT ["/shellcheck"]
