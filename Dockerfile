FROM scratch

LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"

# This file assumes ShellCheck has already been built.
# See https://github.com/koalaman/scbuilder
COPY shellcheck /bin/shellcheck

WORKDIR /mnt
ENTRYPOINT ["/bin/shellcheck"]
