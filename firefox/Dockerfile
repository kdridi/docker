FROM ubuntu:14.04

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates

RUN export uid=1000 gid=1000 && \
    mkdir -p /home/kdridi && \
    echo "kdridi:x:${uid}:${gid}:kdridi,,,:/home/kdridi:/bin/bash" >> /etc/passwd && \
    echo "kdridi:x:${uid}:" >> /etc/group && \
    echo "kdridi ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/kdridi && \
    chmod 0440 /etc/sudoers.d/kdridi && \
    chown ${uid}:${gid} -R /home/kdridi

RUN apt-get install -y firefox

USER kdridi
ENV HOME /home/kdridi
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.0.1
ENV LC_ALL C.UTF-8

CMD firefox
