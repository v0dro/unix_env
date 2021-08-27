#!/usr/bin/env zsh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# oh-my-zsh themes
plugins=(git emacs)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='emacs26'
else
   export EDITOR='emacs26'
fi
alias emacsclient="emacsclient26"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# helpful functions for pytorch dev
pyt_update() {
    git pull --rebase
    git submodule sync --recursive
    git submodule update --init --recursive
    python setup.py develop
    python test/run_test.py
}

pybuild_qs() {
    git add .
    git commit -am "QS dev build `date -c`"
    git push sameer
    ssh qs -o LocalCommand="bash build_pytorch.sh"
}

alias pybuild="DEBUG=1 USE_DISTRIBUTED=0 USE_MKLDNN=1 USE_CUDA=0 BUILD_TEST=0 USE_FBGEMM=0 USE_NNPACK=0 USE_QNNPACK=0 GEN_TO_SOURCE=1 python setup.py develop"

# show hostname and username in prompt or not. comment this line if no need.
# export SHOW_HOST_AND_USER_IN_PROMPT=1

# japanese keyboard
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

# Aliases
alias gs="git status"
alias gp="git push origin master"
alias matlab="/usr/local/bin/matlab"

# wifi commands
alias wifi_list="nmcli device wifi list"
alias wifi_connect="nmcli device wifi connect $1"
alias wifi_new_connect="nmcli d wifi connect $1 password $2"

# battery status
alias batt_stat="upower -i /org/freedesktop/UPower/devices/battery_BAT0"

# ruby tags generator for emacs
alias ruby_tags="ripper-tags -R -e"

# start zotero
alias zot="/home/sameer/Downloads/Zotero_linux-x86_64/zotero"

# OPAM configuration
. /home/sameer/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# oh my zsh prompt
if [ ! -z "$SHOW_HOST_AND_USER_IN_PROMPT" ]; then
  PROMPT="%{$fg[white]%}%n@%{$fg[green]%}%m%{$reset_color%} ${PROMPT}"
fi

# Copy a folder from this machine to a machine over ssh.
#
# Usage: cp_ssh home/folder/name remote_machine_name remote/folder/name
cp_ssh()
{
    ssh_user_name=$(ssh -G $2 | grep "user " | cut -c 6-)
    scp -r $1 $ssh_user_name@$2:$3
}

# add rtags server to PATH
export PATH="$PATH:$HOME/.emacs.d/elpa/rtags-2.21/rtags-2.21/bin/"

# Intel trace analyser
alias itrace="/home/sameer/intel/itac/2019.1.022/bin/traceanalyzer"

# STARPU/PARSEC configurations
export STARPU_DIR=/home/sameer/gitrepos/starpu
export PKG_CONFIG_PATH="${STARPU_DIR}/lib/pkgconfig:${STARPU_DIR}:/home/sameer/gitrepos/parsec/build/parsec/include:/home/sameer/gitrepos/openmpi/lib/pkgconfig"
export LD_LIBRARY_PATH="${STARPU_DIR}/lib:/home/sameer/gitrepos/openmpi/lib:/home/sameer/gitrepos/parsec/build/parsec:${LD_LIBRARY_PATH}"

# YARN binaries
export PAPI_DIR="/home/sameer/gitrepos/papi/src"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${PAPI_DIR}"
export PATH="${PATH}:${PAPI_DIR}/utils"

# nodejs
export PATH="$PATH:/home/sameer/node-v14.16.0-linux-x64/bin"

# nvidia driver
export PATH="$PATH:/usr/local/cuda/bin"


export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if [[ "$(uname -n)" == "sameer-mac.local" ]]; then
    __conda_setup="$('/Users/sameer/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/Users/sameer/opt/anaconda3/etc/profile.d/conda.sh" ]; then
            . "/Users/sameer/opt/anaconda3/etc/profile.d/conda.sh"
        else
            export PATH="/Users/sameer/opt/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
else
    __conda_setup="$('/home/sameer/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/home/sameer/anaconda3/etc/profile.d/conda.sh" ]; then
            . "/home/sameer/anaconda3/etc/profile.d/conda.sh"
        else
            export PATH="/home/sameer/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
fi
# <<< conda initialize <<<


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
