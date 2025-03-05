function hame-env
    set -U PLENV_ROOT $HOME/.plenv
    set -U NODENV_ROOT $HOME/.nodenv

    if [ -d /opt/env ]
        for env in plenv nodenv
            set -Ux (string upper $env)_ROOT /opt/env/$env
        end
    end

    hame-clone -n plenv      -p $PLENV_ROOT                     -u 'tokuhirom/plenv.git'
    hame-clone -n perl-build -p $PLENV_ROOT/plugins/perl-build  -u 'tokuhirom/perl-build.git'
    hame-clone -n nodenv     -p $NODENV_ROOT                    -u 'nodenv/nodenv.git'
    hame-clone -n node-build -p $NODENV_ROOT/plugins/node-build -u 'nodenv/node-build.git'

    path add $PLENV_ROOT/bin
    path add $PLENV_ROOT/shims
    plenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/plenv.fish

    path add $NODENV_ROOT/bin
    path add $NODENV_ROOT/shims
    nodenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/nodenv.fish

end
