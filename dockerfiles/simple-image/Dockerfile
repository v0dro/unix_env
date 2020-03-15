# Base this image on a Ubuntu Xenial installation.
FROM ubuntu:xenial

RUN apt-get update && apt-get install -y \
        sudo \
        curl \
        git \
        bc
# Add a user named 'sameer'.
# RUN useradd -ms /bin/bash sameer
RUN useradd -rm -d /home/sameer -s /bin/bash -g root -G sudo -u 1000 sameer
# RUN adduser --shell /bin/bash sameer \
#         && echo "sameer ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/90-user \
#         && chmod 777 /home/sameer



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

