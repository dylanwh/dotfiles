function hame-env

    hame-clone -n plenv      -p .plenv                    -u 'tokuhirom/plenv.git'
    hame-clone -n perl-build -p .plenv/plugins/perl-build -u 'tokuhirom/perl-build.git'
    hame-clone -n pyenv      -p .pyenv                    -u 'pyenv/pyenv.git'

    path add ~/.plenv/bin
    path add ~/.plenv/shims
    plenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/plenv.fish

    path add ~/.pyenv/bin
    path add ~/.pyenv/shims
    pyenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/pyenv.fish

end

