function hame
    argparse U/update v/verbose n/foreground h/help -- $argv

    if test -n "$_flag_help"
        echo "Usage: hame [-f] [-v] [-n] [-h]"
        echo "  -U, --update: update checkouts when possible"
        echo "  -v, --verbose: print verbose output"
        echo "  -n, --foreground: run hame in the foreground (skip hame-nq)"
        echo "  -h, --help: print this help message"
        return
    end

    set -lx HAME_VERBOSE
    if [ -n "$_flag_verbose" ]
        set HAME_VERBOSE 1
    end

    set -lx HAME_FOREGROUND
    if [ -n "$_flag_foreground" ]
        set HAME_FOREGROUND 1
    end

    set -lx HAME_UPDATE
    if [ -n "$_flag_update" ]
        set HAME_UPDATE yes
    end

    if [ -n $HAME_UPDATE ]
        pushd $HOME/Git/dylanwh/dotfiles
        hame-echo updating "~/Git/dylanwh/dotfiles"
        git pull -q
        perl ~/Git/dylanwh/dotfiles/bin/abraham-linkhome
        local-bin-clean
        popd
    end

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
    user-path clear
    user-path add ~/.local/bin
    user-path add /opt/local/bin
    user-path add /snap/bin

    mkdir -p ~/.cache/hame

    # erase all universal abbreviations
    set -l ver (string split . $FISH_VERSION)
    # fish 3.6.0 or later
    if [ $ver[1] -ge 3 -a $ver[2] -ge 6 ]
        for abbr in (abbr -U --list 2>/dev/null)
            abbr -e $abbr
        end
    end

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
        install-nq
    end

    hame-fq

    hame-env
    hame-git
    hame-vim
    hame-emacs

    hame-rust
    hame-go

    test -d $HOME/.tmux/plugins
    and rm -fr $HOME/.tmux/plugins
    test -d $HOME/.tmux/thumbs-init.conf
    and rm -f $HOME/.tmux/thumbs-init.conf

    if not test -d /nix
        set -l default_perl 5.36.0
        if not [ -d $PLENV_ROOT/versions/$default_perl ]
            hame-echo installing perl $default_perl
            hame-nq plenv install $default_perl
            hame-nq env PLENV_VERSION=$default_perl plenv install-cpanm
            hame-nq plenv global $default_perl
            hame-nq plenv local $default_perl
        end

        if [ -d /opt/node ]
            npm config set prefix /opt/node
            user-path add /opt/node/bin
        end

        # plenv, pyenv, etc should be before /opt/local/bin in the path
        user-path remove /opt/local/bin
        user-path add /opt/local/bin
        user-path remove /snap/bin
        user-path add /snap/bin

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
                $PLENV_ROOT/versions/$default_perl/bin/got >$HOME/.local/bin/got
        end
    end
    hame-echo setting fish_user_path
    set new_fish_user_path $fish_user_path
    set --erase -g fish_user_path
    set -U fish_user_path $new_fish_user_path
    user-path prune

    for func in $hame_after
        hame-echo running $func
        $func
    end
end
