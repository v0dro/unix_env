#!/bin/bash
# Setup a machine with required software.
# This script assumes that the computer uses a Debian environment.
# author: Sameer Deshmukh (@v0dro)

# Checking for git.
if ! hash git 2>/dev/null; then
    echo "Please install git before proceeding."
    exit 1
fi

# Checking for emacs
if ! hash emacs 2>/dev/null; then
    echo "Please install emacs before proceeding."
    exit 1
fi

echo "install packages..."
if [-n "$DESKTOP_ENV"]; then
    sudo apt-get install build-essential git emacs zsh curl texlive texlive-full dirmngr fluxbox rofi net-tools mate-power-manager arandr volumeicon-alsa gnome-screenshot emacs-mozc-bin ibus-mozc
    cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    ~/.dropbox-dist/dropboxd &
fi

echo "Install adobe source code pro fonts..."
mkdir -p ~/.fonts/adobe-fonts/source-code-pro
git clone https://github.com/adobe-fonts/source-code-pro.git ~/.fonts/adobe-fonts/source-code-pro
fc-cache -f -v ~/.fonts/adobe-fonts/source-code-pro

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

# download and intall mendeley
if [-n "$DESKTOP_ENV"]; then
    curl https://desktop-download.mendeley.com/download/apt/pool/main/m/mendeleydesktop/mendeleydesktop_1.18-stable_amd64.deb
    sudo dpkg -i mendeleydesktop_1.18-stable_amd64.deb
    rm mendeleydesktop_1.18-stable_amd64.deb
fi

# install oh my zsh
chsh -s /bin/zsh
cp .zshrc .zshrc_backup
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh
mv .zshrc_backup .zshrc

# TODO:
# * Show machine name in command line so i'll know which machine i'm inside for each shell.
# Something like tsubame@<whatever thing from zsh>. The word 'tsubame' should be taken from
# an env variable that I'll set according to which machine i'm on so it should check for this
# variable before showing this in the cmd.
# * Fix compilation window encoding malfunction in emacs.
# * Have separete environments for remote server and desktop.
#   * DESKTOPS:
#   * SERVERS:
# * Copy terminal setting accross machines
# * Copy mozc keyboard settings across machines, including keyboard shortcuts.

# FOR USER (POST INSTALL TODO):
# * Setup account with firefox.
# * Setup account with thunderbird.
# * Install tokyo tech login info on firefox.
# * Install slack: https://slack.com/downloads/linux
# * Install anaconda: https://www.anaconda.com/download/#linux
# * Install emacs-mozc-bin package.
