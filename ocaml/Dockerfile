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
RUN echo "deb http://download.opensuse.org/repositories/home:/ocaml//Debian_7.0/ /" > /etc/apt/sources.list.d/opam.list
RUN curl -OL http://download.opensuse.org/repositories/home:/ocaml//Debian_7.0/Release.key
RUN apt-key add - < Release.key
RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y dist-upgrade
RUN apt-get -y install aspcud ocaml ocaml-native-compilers camlp4-extra unzip m4 mercurial darcs ncurses-dev pkg-config

RUN git clone -b 1.2 git://github.com/ocaml/opam /tmp/opam
RUN sh -c "cd /tmp/opam && make cold && make install && rm -rf /tmp/opam"
RUN rm -rf /tmp/opam
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
RUN opam init

RUN opam depext -i core
RUN opam depext -i utop
RUN opam depext -i async
RUN opam depext -i core_extended

RUN echo "#use \"topfind\"" >> .ocamlinit && \
	echo "" >> .ocamlinit && \
	echo "#thread" >> .ocamlinit && \
	echo "" >> .ocamlinit && \
	echo "#require \"dynlink\"" >> .ocamlinit && \
	echo "" >> .ocamlinit && \
	echo "#camlp4o" >> .ocamlinit && \
	echo "" >> .ocamlinit && \
	echo "#require \"bin_prot.syntax\"" >> .ocamlinit && \
	echo "#require \"sexplib.syntax\"" >> .ocamlinit && \
	echo "#require \"variantslib.syntax\"" >> .ocamlinit && \
	echo "#require \"fieldslib.syntax\"" >> .ocamlinit && \
	echo "#require \"comparelib.syntax\"" >> .ocamlinit && \
	echo "#require \"core\"" >> .ocamlinit && \
	echo "#require \"async\"" >> .ocamlinit && \
	echo "#require \"core_extended\"" >> .ocamlinit && \
	echo "#require \"core.top\"" >> .ocamlinit && \
	echo "#require \"graphics\"" >> .ocamlinit && \
	echo "" >> .ocamlinit && \
    echo "#use \"main.ml\"" >> .ocamlinit

RUN echo "" >> .zshrc && \
    echo ". \${HOME}/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true" >> .zshrc
################################################################################

WORKDIR /app
CMD zsh
