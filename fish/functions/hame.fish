function hame
    argparse "f/force" -- $argv
    pushd $HOME

    set -U fish_greeting ''
    set -Ux SKIM_DEFAULT_OPTIONS '--preview-window right:70% --bind \'?:toggle-preview,ctrl-o:execute-silent(open {})\''
    set -Ux SKIM_DEFAULT_COMMAND 'fd --type f'
    set -Ux SKIM_ALT_C_COMMAND 'fd -L -t d -E /sys -E /proc -E /dev -E /tmp'
    set -Ux SKIM_CTRL_T_COMMAND 'fd -L -t f -t d -t l -E /sys -E /proc -E /dev -E /tmp'

    set -g fish_user_path $fish_user_path
    path clear
    path add ~/.local/bin
    path add /opt/local/bin

    mkdir -p ~/.cache/hame

    abbr -U -- ps procs
    abbr -U -- ag rg
    abbr -U -- grep rg
    abbr -U -- s srep
    abbr -U -- ci 'git commit'
    abbr -U -- gap 'git add -p'
    abbr -U -- gco 'git checkout'
    abbr -U -- gdc 'git diff --cached'
    abbr -U -- gr 'git rebase'
    abbr -U -- gs 'git status --short'
    abbr -U -- pt 'perltidy --profile=.../.perltidyrc -b  -bext=/'
    abbr -U -- pull 'git pull'
    abbr -U -- push 'git push'
    abbr -U -- runti 'docker run --rm -ti'

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
   set new_fish_user_path $fish_user_path
   set --erase -g fish_user_path
   set -U fish_user_path $new_fish_user_path
   path prune
end

