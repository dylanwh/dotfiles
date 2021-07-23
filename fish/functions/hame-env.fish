function hame-env

    hame-clone -n plenv      -p .plenv                     -u 'tokuhirom/plenv.git'
    hame-clone -n perl-build -p .plenv/plugins/perl-build  -u 'tokuhirom/perl-build.git'
    hame-clone -n pyenv      -p .pyenv                     -u 'pyenv/pyenv.git'
    hame-clone -n nodenv     -p .nodenv                    -u 'nodenv/nodenv.git'
    hame-clone -n node-build -p .nodenv/plugins/node-build -u 'nodenv/node-build.git'

    path add ~/.plenv/bin
    path add ~/.plenv/shims
    plenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/plenv.fish

    path add ~/.pyenv/bin
    path add ~/.pyenv/shims
    pyenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/pyenv.fish

    path add ~/.nodenv/bin
    path add ~/.nodenv/shims
    nodenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/nodenv.fish

end

