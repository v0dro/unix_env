# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Add wisely, as too many plugins slow down shell startup.
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

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

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

# batter status
alias batt_stat="upower -i /org/freedesktop/UPower/devices/battery_BAT0"

# OPAM configuration
. /home/sameer/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
