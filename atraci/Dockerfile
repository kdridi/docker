FROM ubuntu:14.04

RUN apt-get -q update
RUN apt-get -q upgrade -y
RUN apt-get -q install -y sudo pulseaudio vim wget ca-certificates

RUN export uid=1000 gid=1000 && \
    mkdir -p /home/kdridi && \
    echo "kdridi:x:${uid}:${gid}:kdridi,,,:/home/kdridi:/bin/bash" >> /etc/passwd && \
    echo "kdridi:x:${uid}:" >> /etc/group && \
    echo "kdridi ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/kdridi && \
    chmod 0440 /etc/sudoers.d/kdridi && \
    chown ${uid}:${gid} -R /home/kdridi

RUN apt-get install -y libgtk2.0-0 libnss3 libgconf-2-4 libasound2 libxtst6 libnotify4
RUN cp /lib/x86_64-linux-gnu/libudev.so.1 /lib/x86_64-linux-gnu/libudev.so.0

USER kdridi
ENV HOME /home/kdridi
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.42.1
ENV LC_ALL C.UTF-8

RUN cd ~ && \
	wget -c https://github.com/Atraci/Atraci/releases/download/0.7.0/Atraci-linux64.tar.gz && \
	tar -xzf Atraci-linux64.tar.gz && \
	rm -rf Atraci-linux64.tar.gz

CMD ~/linux64/Atraci