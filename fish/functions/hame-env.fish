function hame-env
    set -U plenv_root $HOME/.plenv
    set -U pyenv_root $HOME/.pyenv
    set -U nodenv_root $HOME/.nodenv

    if [ -d /opt/env ]
        for env in plenv pyenv nodenv
            set $env"_root" /opt/env/$env
        end
    end

    hame-clone -n plenv      -p $plenv_root                     -u 'tokuhirom/plenv.git'
    hame-clone -n perl-build -p $plenv_root/plugins/perl-build  -u 'tokuhirom/perl-build.git'
    hame-clone -n pyenv      -p $pyenv_root                     -u 'pyenv/pyenv.git'
    hame-clone -n nodenv     -p $nodenv_root                    -u 'nodenv/nodenv.git'
    hame-clone -n node-build -p $nodenv_root/plugins/node-build -u 'nodenv/node-build.git'

    path add $plenv_root/bin
    path add $plenv_root/shims
    plenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/plenv.fish

    path add $pyenv_root/bin
    path add $pyenv_root/shims
    pyenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/pyenv.fish

    path add $nodenv_root/bin
    path add $nodenv_root/shims
    nodenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/nodenv.fish

end

