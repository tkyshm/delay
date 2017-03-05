FROM alpine:3.5
MAINTAINER tkyshm

ARG ERLANG_VERSION=19.2

LABEL name="erlang" version=$ERLANG_VERSION

ARG ERLANG_DOWNLOAD_URL="https://github.com/erlang/otp/archive/OTP-${ERLANG_VERSION}.tar.gz"
ARG ERLANG_DOWNLOAD_SHA256="c6adbc82a45baa49bf9f5b524089da480dd27113c51b3d147aeb196fdb90516b"

RUN set -xe \
    && apk --update add --virtual build-dependencies curl ca-certificates build-base autoconf perl ncurses-dev openssl-dev unixodbc-dev tar \
    && apk --update add ncurses openssl unixodbc git alpine-sdk\
    && curl -fSL -o otp-src.tar.gz "$ERLANG_DOWNLOAD_URL" \
    && echo "$ERLANG_DOWNLOAD_SHA256  otp-src.tar.gz" | sha256sum -c - \
    && mkdir -p /usr/src/otp-src \
    && tar -xzf otp-src.tar.gz -C /usr/src/otp-src --strip-components=1 \
    && rm otp-src.tar.gz

ARG DISABLED_APPS='megaco wx debugger jinterface orber observer gs et'

RUN set -xe \
    && cd /usr/src/otp-src \
    && for lib in ${DISABLED_APPS} ; do touch lib/${lib}/SKIP ; done \
    && ./otp_build autoconf \
    && ./configure \
        --enable-smp-support \
        --enable-m64-build \
        --disable-native-libs \
        --enable-sctp \
        --enable-threads \
        --enable-kernel-poll \
        --enable-hipe \
        --enable-shared-zlib \
    && make -j$(getconf _NPROCESSORS_ONLN) \
    && make install \
    && find /usr/local -name examples | xargs rm -rf \
    && apk del build-dependencies \
    && ls -d /usr/local/lib/erlang/lib/*/src | xargs rm -rf \
    && rm -rf \
      /opt \
      /var/cache/apk/* \
      /tmp/* \
      /usr/src

RUN set -xe \
    && git clone https://github.com/erlang/rebar3 \
    && cd rebar3 && ./bootstrap \
    && install -m 0755 rebar3 /usr/local/bin \
    && rm -rf /rebar3

ADD init_docker.sh init_docker.sh

ARG CC=gcc
ARG REPO=delay
RUN set -xe\
    && mkdir -p app\
    && cd app\
    && git clone https://github.com/tkyshm/delay\
    && cd $REPO\
    && /usr/local/bin/rebar3 release\
    && ln -sf /app/$REPO/_build/default/rel/delay/bin /app/bin

EXPOSE 9020
CMD ["./init_docker.sh"]
