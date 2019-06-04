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
   export EDITOR='emacs'
else
   export EDITOR='emacs'
fi

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# add rvm to path
export PATH="$PATH:$HOME/.rvm/bin"

# some system specific exports
if [ "$(uname)" "==" "Darwin" ]; then
    # Do something under Mac OS X platform
# export PATH="/Users/sameer/anaconda3/bin:$PATH"  # commented out by conda initialize
elif [ "$(expr substr $(uname -s) 1 5)" "==" "Linux" ]; then
    # Do something under GNU/Linux platform
# export PATH="/home/sameer/anaconda3/bin:$PATH"  # commented out by conda initialize
elif [ "$(expr substr $(uname -s) 1 10)" "==" "MINGW32_NT" ]; then
    # Do something under 32 bits Windows NT platform
elif [ "$(expr substr $(uname -s) 1 10)" "==" "MINGW64_NT" ]; then
    # Do something under 64 bits Windows NT platform
fi

# show hostname and username in prompt or not. comment this line if no need.
# export SHOW_HOST_AND_USER_IN_PROMPT=1

# japanese keyboard
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

# Aliases
alias gs="git status"
alias gp="git push origin master"
alias matlab="/usr/local/MATLAB/R2017b/bin/matlab"

# wifi commands
alias wifi_list="nmcli device wifi list"
alias wifi_connect="nmcli device wifi connect $1"
alias wifi_new_connect="nmcli d wifi connect $1 password $2"

# battery status
alias batt_stat="upower -i /org/freedesktop/UPower/devices/battery_BAT0"

# ruby tags generator for emacs
alias ruby_tags="ripper-tags -R -e"

# OPAM configuration
. /home/sameer/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# oh my zsh prompt
if [ ! -z "$SHOW_HOST_AND_USER_IN_PROMPT" ]; then
  PROMPT="%{$fg[white]%}%n@%{$fg[green]%}%m%{$reset_color%} ${PROMPT}"
fi

alias para="cd ~/gitrepos/scratch/c_shizzle/parallel"
alias rrr="rake clobber && rake compile && rspec"
alias rrrf="rake clobber && rake compile && rspec --t=focus"

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
export STARPU_PATH=/home/sameer/gitrepos/starpu-1.2.8
export PKG_CONFIG_PATH="/home/sameer/gitrepos/starpu-1.2.8/lib/pkgconfig:/home/sameer/gitrepos/starpu-1.2.8:/home/sameer/gitrepos/parsec/build/parsec/include:/home/sameer/gitrepos/openmpi/lib/pkgconfig"
export LD_LIBRARY_PATH="/home/sameer/gitrepos/starpu-1.2.8/lib:/home/sameer/gitrepos/openmpi/lib:/home/sameer/gitrepos/parsec/build/parsec"
export PATH="$STARPU_PATH/bin:$PATH"
export STARPU_FXT_PREFIX="/home/sameer/gitrepos/hicma"
export STARPU_FXT_TRACE=1
export STARPU_GENERATE_TRACE=1

# HiCMA config
export HICMA_INCLUDE="-I/home/sameer/gitrepos/starpu-1.2.8/include/starpu/1.2"
export HICMA_LIB="-L/home/sameer/gitrepos/starpu-1.2.8/lib"
export HICMA_SHARED_LIBS=" -lblas -llapacke "

# YARN binaries
export PATH="$PATH:`yarn global bin`"

# PAPI config
export LD_LIBRARY_PATH="/home/sameer/gitrepos/papi/lib:$LD_LIBRARY_PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
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
# <<< conda initialize <<<

