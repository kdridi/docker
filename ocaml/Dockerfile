FROM ocaml/opam

RUN opam depext -i core
RUN opam depext -i utop
RUN opam depext -i async
RUN opam depext -i core_extended

USER root
ENV USER root
ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y sudo pulseaudio vim wget ca-certificates

RUN apt-get install -y iceweasel

USER opam
ENV USER opam
ENV HOME /home/opam
ENV DISPLAY :0.0
ENV PULSE_SERVER tcp:172.17.0.1
ENV LC_ALL C.UTF-8

WORKDIR $HOME
RUN echo "#use \"topfind\"" >> .ocamlinit && \
    echo "#thread" >> .ocamlinit && \
    echo "#require \"dynlink\"" >> .ocamlinit && \
    echo "#camlp4o" >> .ocamlinit && \
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
    echo "#use \"main.ml\"" >> .ocamlinit

WORKDIR /app
CMD utop
