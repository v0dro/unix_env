# Docker image for working on pytorch with Ubuntu Xenial (16.04)
# and CUDA 10.2.
#
# Reproduces the following environment:

# Mar 15 05:08:32 PyTorch built with:
# Mar 15 05:08:32   - GCC 7.4
# Mar 15 05:08:32   - C++ Version: 201402
# Mar 15 05:08:32   - Intel(R) Math Kernel Library Version 2020.0.0 Product Build 20191122 for Intel(R) 64 architecture applications
# Mar 15 05:08:32   - Intel(R) MKL-DNN v0.21.1 (Git Hash 7d2fd500bc78936d1d648ca713b901012f470dbc)
# Mar 15 05:08:32   - OpenMP 201511 (a.k.a. OpenMP 4.5)
# Mar 15 05:08:32   - NNPACK is enabled
# Mar 15 05:08:32   - CPU capability usage: AVX2
# Mar 15 05:08:32   - CUDA Runtime 10.2
# Mar 15 05:08:32   - NVCC architecture flags: -gencode;arch=compute_52,code=sm_52
# Mar 15 05:08:32   - CuDNN 7.6.5
# Mar 15 05:08:32   - Build settings: BLAS=MKL, BUILD_TYPE=Release, CXX_FLAGS= -Wno-deprecated -fvisibility-inlines-hidden -fopenmp -DNDEBUG -DUSE_FBGEMM -DUSE_QNNPACK -DUSE_PYTORCH_QNNPACK -DUSE_XNNPACK -O2 -fPIC -Wno-narrowing -Wall -Wextra -Wno-missing-field-initializers -Wno-type-limits -Wno-array-bounds -Wno-unknown-pragmas -Wno-sign-compare -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -Wno-unused-result -Wno-strict-overflow -Wno-strict-aliasing -Wno-error=deprecated-declarations -Wno-stringop-overflow -Wno-error=pedantic -Wno-error=redundant-decls -Wno-error=old-style-cast -fdiagnostics-color=always -faligned-new -Werror -Wno-unused-but-set-variable -Wno-maybe-uninitialized -fno-math-errno -fno-trapping-math -Werror=format -Wno-stringop-overflow, PERF_WITH_AVX=1, PERF_WITH_AVX2=1, PERF_WITH_AVX512=1, USE_CUDA=ON, USE_EXCEPTION_PTR=1, USE_GFLAGS=OFF, USE_GLOG=OFF, USE_MKL=ON, USE_MKLDNN=ON, USE_MPI=OFF, USE_NCCL=ON, USE_NNPACK=ON, USE_OPENMP=ON, USE_STATIC_DISPATCH=OFF, 
# Mar 15 05:08:32


# Usage: docker run --gpus all pytorch-dev:v2 bash -l test.sh


# Base this image on a Ubuntu Xenial installation with CUDA 10.2 and CUDNN 7.6.5.
FROM nvidia/cuda:10.2-cudnn7-devel-ubuntu16.04

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

# Copy SSH key for git private repos
# ADD ~/.ssh/id_rsa /root/.ssh/id_rsa
# RUN chmod 600 /root/.ssh/id_rsa # Use git with SSH instead of https
# RUN echo \"StrictHostKeyChecking no \" > /root/.ssh/config

# Download and install miniconda.
RUN curl -so ~/miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
        && chmod +x ~/miniconda.sh \
        && ~/miniconda.sh -b -p ~/miniconda \
        && rm ~/miniconda.sh \
        && ~/miniconda/bin/conda init bash
        
ENV PATH=/home/sameer/miniconda/bin:$PATH
ENV CONDA_AUTO_UPDATE_CONDA=false

# Setup conda pytorch xenial environment.
COPY pytorch-xenial-env.yaml .
ENV CONDA_ENV=pytorch-xenial-env
RUN conda env create --file=pytorch-xenial-env.yaml -n $CONDA_ENV && conda clean -ya
RUN conda init bash

RUN git clone https://github.com/pytorch/pytorch.git \
        && cd pytorch \
        && git remote add sameer https://github.com/v0dro/pytorch.git \
        && git fetch --all \
        && git submodule sync --recursive \
        && git submodule update --init --recursive


# copy all the files to the container
COPY build.sh .

# install dependencies
RUN /bin/bash -l build.sh

