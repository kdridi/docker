FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates build-essential git command-not-found zsh gdb software-properties-common

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    git clone git://github.com/robbyrussell/oh-my-zsh.git ${home}/.oh-my-zsh && \
    echo "${login}:x:${uid}:${gid}:${login},,,:${home}:/bin/bash" >> /etc/passwd && \
    echo "${login}:x:${uid}:" >> /etc/group && \
    echo "${login} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${login} && \
    chmod 0440 /etc/sudoers.d/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
COPY zshrc.conf .zshrc

USER root
ENV USER root
ENV HOME /$USER
ENV DEBIAN_FRONTEND noninteractive

WORKDIR $HOME
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google.list
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y
RUN apt-get update -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y google-chrome-stable

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.0.1
ENV LC_ALL C.UTF-8

WORKDIR $HOME
CMD zsh
