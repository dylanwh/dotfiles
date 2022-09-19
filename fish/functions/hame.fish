function hame
    argparse "f/force" "v/verbose" "n/foreground" -- $argv

    set -lx HAME_FLAGS ""
    if [ -n "$_flag_force" ]
        set HAME_FLAGS "--force"
    end

    set -lx HAME_VERBOSE
    if [ -n "$_flag_verbose" ]
        set HAME_VERBOSE 1
    end

    set -lx HAME_FOREGROUND
    if [ -n "$_flag_foreground" ]
        set HAME_FOREGROUND 1
    end

    pushd $HOME/Git/dylanwh/dotfiles
    hame-echo updating "~/Git/dylanwh/home"
    git pull -q
    popd

    pushd $HOME
    hame-echo configuring fish
    set -U fish_greeting ''
    set -Ux SKIM_DEFAULT_OPTIONS '--preview-window right:70% --bind \'?:toggle-preview,ctrl-o:execute-silent(open {})\''
    set -Ux SKIM_DEFAULT_COMMAND 'fd --type f'
    set -Ux SKIM_ALT_C_COMMAND 'fd -L -t d -E /sys -E /proc -E /dev -E /tmp'
    set -Ux SKIM_CTRL_T_COMMAND 'fd -L -t f -t d -t l -E /sys -E /proc -E /dev -E /tmp'
    set -Ux SKIM_CTRL_T_OPTS '--preview \'preview.sh {}\''
    set -Ux SKIM_TMUX 1

    set -g fish_user_path $fish_user_path
    path clear
    path add ~/.local/bin
    path add /opt/local/bin
    path add /snap/bin

    mkdir -p ~/.cache/hame

    abbr -Uq ps; and abbr -U --erase ps
    abbr -U -- ag rg
    abbr -U -- grep rg
    abbr -U -- yo yoink
    abbr -Uq s; and abbr -U --erase s
    abbr -U -- rm 'rm -i'
    abbr -U -- mv 'mv -i'
    abbr -U -- cp 'cp -i'
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
    abbr -U -- upstream 'git push --set-upstream origin (git branch --show-current)'
    abbr -U -- hm history merge


    switch $OS
        case Darwin
            hame-macos
        case Linux
            [ -f /etc/debian_version ]
            and hame-debian
            [ -f /etc/alpine-release ]
            and hame-alpine
    end

    if not have nq
        hame-echo installing nq
        install_nq
    end

    hame-fq

    hame-env
    hame-git
    hame-vim
    hame-emacs

    hame-rust
    hame-go
    hame-tmux

    set -l default_perl 5.34.0
    if not [ -d $PLENV_ROOT/versions/$default_perl ]
        hame-echo installing perl $default_perl
        hame-nq plenv install $default_perl
        hame-nq env PLENV_VERSION=$default_perl plenv install-cpanm
        hame-nq plenv global $default_perl
        hame-nq plenv local $default_perl
    end

    if not have node
        set -l default_node 16.5.0
        if not [ -d $NODENV_ROOT/versions/$default_node ]
            hame-echo installing node $default_node
            hame-nq nodenv install $default_node
            hame-nq nodenv global $default_node
            hame-nq nodenv local $default_node
        end
    end

    if [ -d /opt/node ]
        npm config set prefix /opt/node
        path add /opt/node/bin
    end

    # plenv, pyenv, etc should be before /opt/local/bin in the path
    path remove /opt/local/bin
    path add /opt/local/bin
    path remove /snap/bin
    path add /snap/bin

    if not perl -MMojolicious -e 1 &>/dev/null
        hame-echo installing Mojolicious
        hame-nq cpanm --notest Mojolicious
    end
    if not [ -f $HOME/.local/bin/got ]
        hame-echo installing gitgot
        hame-nq cpanm --notest App::GitGot
        hame-nq cpanm --notest Path::Iterator::Rule
        hame-nq cpanm --notest JSON
        hame-nq sed "1 s|^#!.*|#!$PLENV_ROOT/versions/$default_perl/bin/perl|" \
            $PLENV_ROOT/versions/$default_perl/bin/got > $HOME/.local/bin/got
    end

    popd
    hame-echo setting fish_user_path
    set new_fish_user_path $fish_user_path
    set --erase -g fish_user_path
    set -U fish_user_path $new_fish_user_path
    path prune
end

