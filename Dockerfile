FROM ubuntu:14.04
MAINTAINER Akira Baruah <akira.baruah@gmail.com>

RUN apt-get update && apt-get install -y \
    emacs \
    git \
    make \
    man \
    ocaml \
    vim

RUN git clone https://github.com/teamSEAM/ProjectSEAM.git
WORKDIR /ProjectSEAM
