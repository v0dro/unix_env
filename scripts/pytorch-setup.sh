# Various scripts for pytorch setup on qgpu and other machines.
print_environment() {
    echo "---- DONE ENVIRONMENT SETUP ----"
    echo "================================"
    echo "Using C compiler: "
    echo `${CC} --version`

    echo "Using C++ compiler: "
    echo `${CXX} --version`

    echo "CXXFLAGS: ${CXXFLAGS}"
    echo "CFLAGS: ${CFLAGS}"
    echo "LDFLAGS: ${LDFLAGS}"
    echo "================================"
}

pytorch_do_pre_setup() {
    # conda deactivate
    unset CXXFLAGS
    unset CFLAGS
    unset LDFLAGS
}

# Update pytorch submodules from upstream.
pytorch_update_modules() {
    . /usr/local/cuda/env.sh
    git pull --rebase
    git submodule sync --recursive
    git submodule update --init --recursive    
}

# Prepare environment.
pytorch_setup_env() {
    export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH
    export CXXFLAGS="`echo $CXXFLAGS | sed 's/-std=c++17/-std=c++14/'`"
    export CFLAGS="$CFLAGS -L$CONDA_PREFIX/lib"
    export CXXFLAGS="$CXXFLAGS -L$CONDA_PREFIX/lib -L$CUDA_HOME/lib64 -lcusparse -lcublas "
    export USE_NCCL=0
    export USE_CUDA=1
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu:$CUDA_HOME/lib64"
    export LDFLAGS="${LDFLAGS} -Wl,-rpath,${CUDA_HOME}/lib64 -Wl,-rpath-link,${CUDA_HOME}/lib64 -L${CUDA_HOME}/lib64"
    export MAX_JOBS=20    
}

pytorch_install() {
    pytorch_setup_env
    python setup.py install
}

pytorch_setup_basic_environment() {
    pytorch_update_modules
    pytorch_setup_env
}

# Setup pytorch with ASAN and clang5. Use the 
pytorch_setup_clang5() {
    pytorch_do_pre_setup
    conda activate pytorch-clang-dev
    pytorch_setup_env
    export CC=${CONDA_PREFIX}/bin/clang
    export CXX=${CONDA_PREFIX}/bin/clang++
    # pytorch_update_modules
    # print_environment
    # pytorch_install
}

simple_s() {
    conda activate pytorch-clang-dev
    python setup.py install

}

# Setup pytorch with Pearu's CUDA environment.
pytorch_setup_cuda() {
    pytorch_do_pre_setup
    conda activate pytorch-cuda-dev
    export CC=${CONDA_PREFIX}/bin/gcc
    export CXX=${CONDA_PREFIX}/bin/g++
    pytorch_update_modules
    print_environment
    pytorch_install
}

pytorch_setup_cuda() {
    
}

pytorch_check_linting() {
    
}
