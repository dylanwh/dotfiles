function hame-env

    hame-git -n plenv      -p .plenv                    -u 'tokuhirom/plenv.git'
    hame-git -n perl-build -p .plenv/plugins/perl-build -u 'tokuhirom/perl-build.git'
    hame-git -n pyenv      -p .pyenv                    -u 'pyenv/pyenv.git'

    path add ~/.plenv/bin
    plenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/plenv.fish

    path add ~/.pyenv/bin
    pyenv init - fish | grep -v 'set -gx PATH' > ~/.config/fish/functions/pyenv.fish

end

