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
RUN apt-get install -y git
RUN apt-get install -y gconf2
RUN apt-get install -y gconf-service
RUN apt-get install -y libgtk2.0-0
RUN apt-get install -y libnotify4
RUN apt-get install -y libxtst6
RUN apt-get install -y libnss3
RUN apt-get install -y python
RUN apt-get install -y gvfs-bin
RUN apt-get install -y xdg-utils

RUN wget -c https://edgehill.s3-us-west-2.amazonaws.com/0.4.5-b0ec335/linux-deb/x64/N1.deb
RUN dpkg -i N1.deb
RUN rm -rf N1.deb

# RUN apt-get install -y npm
# RUN apt-get install -y nodejs-legacy
# RUN npm install electron-prebuilt -g

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.0.1
ENV LC_ALL C.UTF-8

WORKDIR $HOME
RUN wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | bash

ENV NVM_DIR $HOME/.nvm
RUN bash -c "source $NVM_DIR/nvm.sh && nvm ls-remote && nvm install v5.6.0 && nvm use v5.6.0"
RUN bash -c "source $NVM_DIR/nvm.sh && nvm use v5.6.0 && npm install electron-prebuilt -g"

ENV ELECTRON_PATH /home/kdridi/.nvm/versions/node/v5.6.0/bin/electron

CMD /usr/share/nylas/nylas
