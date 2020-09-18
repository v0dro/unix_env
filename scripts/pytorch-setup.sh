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
    git pull --rebase
    git submodule sync --recursive
    git submodule update --init --recursive    
}

# Prepare environment.
# pytorch_setup_env USE_CUDA
pytorch_setup_env() {
    export DEBUG=1
    export USE_CUDA=1
    export USE_NCCL=0
    export USE_NCCL=0
    export MAX_JOBS=20
    export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH
    export CXXFLAGS="`echo $CXXFLAGS | sed 's/-std=c++17/-std=c++14/'`"
    export CFLAGS="$CFLAGS -L$CONDA_PREFIX/lib"
    export CXXFLAGS="$CXXFLAGS -L$CONDA_PREFIX/lib "
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu"

    if test -f /usr/local/cuda/env.sh; then
        . /usr/local/cuda/env.sh
    fi
    
    if [[ "$WITH_CLANG" -eq "1" ]]; then
        export CC=${CONDA_PREFIX}/bin/clang
        export CXX=${CONDA_PREFIX}/bin/clang++
        export OpenMP_C_FLAGS=" -fopenmp=libomp  -Wno-unused-command-line-argument "
        export OpenMP_C_LIB_NAMES="libomp" "libgomp" "libiomp5"
        export OpenMP_CXX_FLAGS=" -fopenmp=libomp -Wno-unused-command-line-argument "
        export OpenMP_CXX_LIB_NAMES="libomp" "libgomp" "libiomp5"
    else
        export CC=/usr/bin/gcc
        export CXX=/usr/bin/g++
    fi

    if [ "$USE_CUDA" -eq "1" ]; then
        export CXXFLAGS="$CXXFLAGS -L${CUDA_HOME}/lib64 -lcusparse -lcublas "
        export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${CUDA_HOME}/lib64"
        export LDFLAGS="${LDFLAGS} -Wl,-rpath,${CUDA_HOME}/lib64 -Wl,-rpath-link,${CUDA_HOME}/lib64 -L${CUDA_HOME}/lib64 -lrt -lpthread -lresolv "
    fi

    export CMAKE_PREFIX_PATH=${CONDA_PREFIX:-"$(dirname $(which conda))/../"}
    python setup.py build --cmake-only
    ccmake build  # or cmake-gui build
}

pytorch_install() {
    pytorch_setup_env
    python setup.py develop
}

pytorch_setup_basic_environment() {
    pytorch_update_modules
    pytorch_setup_env
}

export -f pytorch_install
