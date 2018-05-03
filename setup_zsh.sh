# install oh my zsh
chsh -s /bin/zsh
cp .zshrc .zshrc_backup
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
mv .zshrc_backup .zshrc
