FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates build-essential git command-not-found zsh gdb software-properties-common curl

ENV CONF_USER kdridi
ENV CONF_HOME /home/$CONF_USER
ENV CONF_UUID 1000
ENV CONF_UGID 1000

RUN git clone git://github.com/robbyrussell/oh-my-zsh.git ${CONF_HOME}/.oh-my-zsh && \
    echo "${CONF_USER}:x:${CONF_UUID}:${CONF_UGID}:${CONF_USER},,,:${CONF_HOME}:/bin/bash" >> /etc/passwd && \
    echo "${CONF_USER}:x:${CONF_UUID}:" >> /etc/group && \
    echo "${CONF_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${CONF_USER} && \
    chmod 0440 /etc/sudoers.d/${CONF_USER} && \
    mkdir /app && \
    chown ${CONF_UUID}:${CONF_UGID} -R /app

COPY zshrc.conf ${CONF_HOME}/.zshrc
COPY ocamlinit.conf ${CONF_HOME}/.ocamlinit

RUN chown ${CONF_UUID}:${CONF_UGID} -R ${CONF_HOME}

RUN echo "deb http://download.opensuse.org/repositories/home:/ocaml//Debian_7.0/ /" > /etc/apt/sources.list.d/opam.list
RUN curl -OL http://download.opensuse.org/repositories/home:/ocaml//Debian_7.0/Release.key
RUN apt-key add - < Release.key
RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y dist-upgrade
RUN apt-get -y install m4 unzip ncurses-dev aspcud ocaml ocaml-native-compilers camlp4-extra

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN git clone -b 1.2 git://github.com/ocaml/opam /tmp/opam
RUN sh -c "cd /tmp/opam && make cold && make install && rm -rf /tmp/opam"
RUN rm -rf /tmp/opam

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
RUN opam init
RUN opam depext -i core
RUN opam depext -i utop
RUN opam depext -i async
RUN opam depext -i core_extended

WORKDIR $HOME
RUN echo "" >> .ocamlinit && \
    echo "#use \"main.ml\"" >> .ocamlinit

RUN echo "" >> .zshrc && \
    echo ". \${HOME}/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true" >> .zshrc

WORKDIR /app
CMD zsh