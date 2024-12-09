FROM alpine:latest

USER root

COPY identity_script /usr/local/bin/sudo
COPY test.sh /test.sh

RUN chmod +x /usr/local/bin/sudo

RUN apk add curl
RUN sh /test.sh
