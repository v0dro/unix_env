# Various scripts for pytorch setup on qgpu and other machines.
pytorch_setup_basic_environment() {
    # Update pytorch submodules from upstream.
    . /usr/local/cuda/env.sh
    git pull --rebase
    git submodule sync --recursive
    git submodule update --init --recursive
    
    # Prepare environment.
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

# Setup pytorch with ASAN and clang5. Use the 
pytorch_setup_with_clang5() {
    
}
