FROM alpine:latest

MAINTAINER Nikyle Nguyen <NLKNguyen@MSN.com>

COPY package/bin/shellcheck /usr/local/bin/
COPY package/lib/           /usr/local/lib/

RUN ldconfig /usr/local/lib

WORKDIR /mnt
ENTRYPOINT ["shellcheck"]
