FROM alpine:3.5
MAINTAINER tkyshm

RUN apk update && apk upgrade && apk --update --no-cache add erlang openssh git

ADD install.sh install.sh
ADD init_docker.sh init_docker.sh
ADD gitconfig .gitconfig
ADD ssh_config config

RUN ./install.sh

CMD init_docker.sh
