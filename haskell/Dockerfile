FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

COPY 01proxy /etc/apt/apt.conf.d/01proxy

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y build-essential ca-certificates command-not-found curl gdb git manpages manpages-dev manpages-fr manpages-fr-dev manpages-fr-extra pulseaudio software-properties-common sudo vim wget zsh

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
RUN apt-get install -y ghc

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN apt-get clean -y

RUN export uid=1000 gid=1000 && login=kdridi && home=/home/${login} && \
    chown ${uid}:${gid} -R ${home}

USER kdridi
ENV USER kdridi
ENV HOME /home/$USER

WORKDIR $HOME
RUN rm -rf .zcompdump*
RUN rm -rf .zsh-update
RUN rm -rf .zsh_history
RUN sed -i .zshrc -e 's,^plugins=(,plugins=(ubuntu ,'
RUN sed -i .zshrc -e 's,^plugins=(,plugins=(cabal ,'
RUN cat .zshrc

WORKDIR /app
CMD [ "ghci", "Main.hs" ]
