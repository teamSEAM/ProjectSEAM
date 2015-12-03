FROM ubuntu:14.04
MAINTAINER Akira Baruah <akira.baruah@gmail.com>

RUN apt-get update && apt-get install -y \
    emacs \
    git \
    make \
    man \
    ocaml \
    vim

WORKDIR /ProjectSEAM
COPY . /ProjectSEAM
