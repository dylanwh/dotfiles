function hame-git
   git config --global push.default simple
   git config --global user.email $EMAIL
   git config --global user.name "$REALNAME"
   git config --global alias.co checkout
   git config --global alias.ci commit
   git config --global alias.br branch
   git config --global alias.st "status --short --branch"
   git config --global alias.up "!git fetch --all --tags --prune && git pull --rebase"
   git config --global alias.reword "commit --amend"
   git config --global alias.top "!pwd"
   git config --global color.ui auto
   git config --global color.diff auto
   git config --global color.status auto
   git config --global color.branch auto
   git config --global core.excludesfile "~/.cvsignore"
   git config --global pull.ff only
end

