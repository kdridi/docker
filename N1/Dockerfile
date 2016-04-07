FROM kdridi/base:ubuntu-14.04-r3
MAINTAINER Karim DRIDI <karim.dridi@gmail.com>

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y

WORKDIR $HOME
# SYSTEM #######################################################################
RUN apt-get install -y gir1.2-gnomekeyring-1.0 gconf2 gconf-service libgtk2.0-0 libnotify4 libxtst6 libnss3 python gvfs-bin xdg-utils
RUN wget -c https://edgehill.s3-us-west-2.amazonaws.com/0.4.19-d41e72c/linux-deb/x64/N1.deb
RUN dpkg -i N1.deb
RUN rm -rf N1.deb
################################################################################

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN rm -rf /etc/apt/apt.conf.d/01proxy

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
# USER #########################################################################
RUN wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | bash

ENV NVM_DIR $HOME/.nvm
ENV NODE_VERSION v5.10.1
RUN bash -c "source $NVM_DIR/nvm.sh && nvm ls-remote && nvm install $NODE_VERSION && nvm use $NODE_VERSION"
RUN bash -c "source $NVM_DIR/nvm.sh && nvm use $NODE_VERSION && npm install electron-prebuilt -g"

ENV ELECTRON_PATH /home/kdridi/.nvm/versions/node/$NODE_VERSION/bin/electron
################################################################################

WORKDIR $HOME
CMD /usr/share/nylas/nylas