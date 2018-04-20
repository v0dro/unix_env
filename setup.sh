#!/bin/bash
# Setup a machine with required software.
# author: Sameer Deshmukh (@v0dro)

if [-z "$SERVER_ENV"] && [-z "$DESKTOP_ENV"]; then # neither vars are set
    echo "Set SERVER_ENV or DESKTOP_ENV"
    exit 1
fi

if [-n "$SERVER_ENV"]; then # SERVER_ENV is set
    
elif [-n "$DESKTOP_ENV"]; then # DESKTOP_ENV is set
    
fi

# install rvm and ruby
echo "Installing rvm and ruby.."
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
curl -sSL https://get.rvm.io | bash -s stable --ruby

# make gitrepos folder and download some repos
echo "Cloning gitrepos..."
mkdir gitrepos
cd gitrepos
git clone https://github.com/v0dro/scratch
cd

# install oh my zsh
echo "Installing oh my zsh.."
chsh -s /bin/zsh
cp .zshrc .zshrc_backup
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh
mv .zshrc_backup .zshrc

# TODO:
# * Have a way to clone my zshrc wihthout having oh my zsh interfere with it.
# * The path of ohmyzsh in the zshrc changes according to the user name.
# * Show machine name in command line so i'll know which machine i'm inside for each shell.
# Something like tsubame@<whatever thing from zsh>. The word 'tsubame' should be taken from
# an env variable that I'll set according to which machine i'm on so it should check for this
# variable before showing this in the cmd.
# * Fix compilation window encoding malfunction in emacs.
# * Have separete environments for remote server and desktop.
#   * DESKTOPS:
#     * Install slack.
#   * SERVERS:
# * Copy terminal setting accross machines
# * Copy mozc keyboard settings across machines, including keyboard shortcuts.

# FOR USER (POST INSTALL TODO):
# * Setup account with firefox.
# * Setup account with thunderbird.
# * Install tokyo tech login info on firefox.
# * 
