function hame-git
    hame-echo configuring git
    set gitconfig ~/.gitconfig
    cp $gitconfig $gitconfig~
    git config --global color.branch auto
    git config --global color.diff auto
    git config --global color.status auto
    git config --global color.ui auto
    git config --global core.excludesfile "~/.cvsignore"
    git config --global diff.colorMoved default
    git config --global init.defaultBranch main
    git config --global pull.ff only
    git config --global push.default simple
    git config --global user.email "$EMAIL"
    git config --global user.name "$REALNAME"

    git config --global alias.br branches
    git config --global alias.ci commit
    git config --global alias.co checkout
    git config --global alias.ndiff '!env DELTA_NAVIGATE=1 git diff'
    git config --global alias.reword "commit --amend"
    git config --global alias.st "status --short --branch"
    git config --global alias.top "!pwd"
    git config --global alias.up "!git fetch --all --tags --prune && git pull --rebase"

    git config --global url."https://github.com/".insteadOf git://github.com/

    if have delta
        hame-echo configuring git to use delta
        git config --global core.pager (which delta)
        git config --global interactive.diffFilter "delta --color-only"
        git config --global delta.features 'side-by-side line-numbers decorations'
        git config --global delta.whitespace-error-style '22 reverse'
        git config --global delta.decorations.commit-decoration-style 'bold yellow box ul'
        git config --global delta.decorations.file-style 'bold yellow ul'
        git config --global delta.decorations.file-decoration-style none
    end

    test -n "$HAME_VERBOSE"
    or return 0

    if have delta
        delta $gitconfig~ $gitconfig
    else
        diff -u $gitconfig~ $gitconfig
    end
end
