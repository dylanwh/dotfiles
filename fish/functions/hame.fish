function hame
    argparse "f/force" -- $argv
    pushd $HOME

    set -U fish_greeting ''
    set -Ux SKIM_DEFAULT_OPTIONS '--preview-window right:70% --bind \'?:toggle-preview,ctrl-o:execute-silent(open {})\''

    path clear
    path add ~/.local/bin
    path add /opt/local/bin

    abbr -a -U -- ag rg
    abbr -a -U -- grep rg
    abbr -a -U -- ci 'git commit'
    abbr -a -U -- gap 'git add -p'
    abbr -a -U -- gco 'git checkout'
    abbr -a -U -- gdc 'git diff --cached'
    abbr -a -U -- gr 'git rebase'
    abbr -a -U -- gs 'git status --short'
    abbr -a -U -- pt 'perltidy --profile=.../.perltidyrc -b  -bext=/'
    abbr -a -U -- pull 'git pull'
    abbr -a -U -- push 'git push'
    abbr -a -U -- runti 'docker run --rm -ti'

    set -lx HAME_FLAGS ""
    if [ $_flag_force ]
        set HAME_FLAGS "--force"
    end

    switch $OS
        case Darwin
            hame-macos
    end

    if not have mosh
        echo installing mosh
        install_mosh
    end
    if not have nq
        echo installing nq
        install_nq
    end

    hame-fq

    hame-env
    hame-vim
    hame-emacs

    hame-rust
    if have go
        path add $GOPATH/bin
        if not have gore
            echo installing gore
            hame-nq go get github.com/k0kubun/pp
            hame-nq go get github.com/mdempsky/gocode
            hame-nq go get github.com/motemen/gore/cmd/gore
        end
    end

    set -l default_perl 5.30.2
    if not [ -d ~/.plenv/versions/$default_perl ]
        hame-nq plenv install $default_perl
        hame-nq env PLENV_VERSION=$default_perl plenv install-cpanm
        hame-nq plenv global $default_perl
        hame-nq plenv local $default_perl
    end
    if not perl -MMojolicious -e 1 &>/dev/null
        hame-nq cpanm --notest Mojolicious
    end

    # plenv, pyenv, etc should be before /opt/local/bin in the path
    path remove /opt/local/bin
    path add /opt/local/bin

   selenized
   popd
   path prune
end

