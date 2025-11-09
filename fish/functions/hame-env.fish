function hame-env
    test -d /nix
    and return 0

    set -U PLENV_ROOT $HOME/.plenv

    if [ -d /opt/env ]
        for env in plenv
            set -Ux (string upper $env)_ROOT /opt/env/$env
        end
    end

    hame-clone -n plenv -p $PLENV_ROOT -u 'tokuhirom/plenv.git'
    hame-clone -n perl-build -p $PLENV_ROOT/plugins/perl-build -u 'tokuhirom/perl-build.git'

    user-path add $PLENV_ROOT/bin
    user-path add $PLENV_ROOT/shims
    plenv init - fish | grep -v 'set -gx PATH' >~/.config/fish/functions/plenv.fish

    if have fnm
        source (fnm env | sed '/set -gx PATH/ d; s/set -gx/set -Ux/' | psub)
        user-path add $FNM_MULTISHELL_PATH/bin
    end
end
