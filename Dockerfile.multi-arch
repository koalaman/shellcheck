# Alpine image
FROM alpine:latest AS alpine
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
ARG tag

# Put the right binary for each architecture into place for the
# multi-architecture docker image.
RUN set -x; \
  arch="$(uname -m)"; \
  echo "arch is $arch"; \
  if [ "${arch}" = 'armv7l' ]; then \
    arch='armv6hf'; \
  fi; \
  url_base='https://github.com/koalaman/shellcheck/releases/download/'; \
  tar_file="${tag}/shellcheck-${tag}.linux.${arch}.tar.xz"; \
  wget "${url_base}${tar_file}" -O - | tar xJf -; \
  mv "shellcheck-${tag}/shellcheck" /bin/; \
  rm -rf "shellcheck-${tag}"; \
  ls -laF /bin/shellcheck

# ShellCheck image
FROM scratch
LABEL maintainer="Vidar Holen <vidar@vidarholen.net>"
WORKDIR /mnt
COPY --from=alpine /bin/shellcheck /bin/
ENTRYPOINT ["/bin/shellcheck"]
