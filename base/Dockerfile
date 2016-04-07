FROM ubuntu:14.04
MAINTAINER Karim DRIDI <karim.dridi@gmail.com>

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y build-essential ca-certificates command-not-found curl gdb git manpages manpages-dev manpages-fr manpages-fr-dev manpages-fr-extra pulseaudio software-properties-common sudo vim wget zsh

ENV CONF_USER kdridi
ENV CONF_HOME /home/$CONF_USER
ENV CONF_NAME "Karim DRIDI"
ENV CONF_MAIL "karim.dridi@gmail.com"
ENV CONF_UUID 1000
ENV CONF_UGID 1000

RUN git clone git://github.com/robbyrussell/oh-my-zsh.git ${CONF_HOME}/.oh-my-zsh && \
    echo "${CONF_USER}:x:${CONF_UUID}:${CONF_UGID}:${CONF_NAME},,,:${CONF_HOME}:/bin/bash" >> /etc/passwd && \
    echo "${CONF_USER}:x:${CONF_UUID}:" >> /etc/group && \
    echo "${CONF_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${CONF_USER} && \
    chmod 0440 /etc/sudoers.d/${CONF_USER} && \
    mkdir /app && \
    chown ${CONF_UUID}:${CONF_UGID} -R /app

COPY zshrc.conf ${CONF_HOME}/.zshrc
RUN sed -i ${CONF_HOME}/.zshrc -e 's,^plugins=(,plugins=(ubuntu ,'

RUN chown ${CONF_UUID}:${CONF_UGID} -R ${CONF_HOME}

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

USER $CONF_USER
ENV USER $CONF_USER
ENV HOME $CONF_HOME

WORKDIR $HOME
RUN git config --global user.name "$CONF_NAME"
RUN git config --global user.email "$CONF_MAIL"

CMD zsh
