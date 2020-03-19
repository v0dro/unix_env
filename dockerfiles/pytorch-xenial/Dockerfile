# Docker image for working on pytorch with Ubuntu Xenial (16.04)
# and CUDA 10.2.
#
# Usage: docker run --gpus all pytorch-dev:v2 bash -l test.sh

# Base this image on a Ubuntu Xenial installation.
FROM nvidia/cuda:10.2-devel-ubuntu16.04

ENV CUDA_HOME=/usr/local/cuda/

RUN apt-get update && apt-get install -y \
        sudo \
        curl \
        git \
        bc
# Add a user named 'sameer'.
RUN useradd -rm -d /home/sameer -s /bin/bash -g root -G sudo -u 1000 sameer

# https://docs.docker.com/engine/reference/builder/#user
USER sameer
ENV HOME=/home/sameer

# https://docs.docker.com/engine/reference/builder/#workdir
# Sets the working directory for all the instructions that follow this directory.
# Can also use ENV variables set in the Dockerfile.
WORKDIR /home/sameer

# Download and install miniconda.
RUN curl -so ~/miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
        && chmod +x ~/miniconda.sh \
        && ~/miniconda.sh -b -p ~/miniconda \
        && rm ~/miniconda.sh \
        && ~/miniconda/bin/conda init bash

# copy all the files to the container
COPY build.sh .

# install dependencies
RUN /bin/bash -l build.sh

