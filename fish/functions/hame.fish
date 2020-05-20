function hame
    argparse "f/force" -- $argv
    cd $HOME

    set -lx HAME_FLAGS ""
    if [ $_flag_force ]
        set HAME_FLAGS "--force"
    end

    switch $OS
        case Darwin
            test -f /etc/sudoers.d/99-port
            or echo 'dylan ALL = (root) NOPASSWD: /opt/local/bin/port' | sudo tee /etc/sudoers.d/99-port

            if not have fzy
                sudo port install fzy
            end
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

    if have cargo
        path add ~/.cargo/bin

        have exa
        or hame-nq cargo install exa
        have vivid
        or hame-nq cargo install vivid
        have fd
        or hame-nq cargo install fd-find
        have rg
        or hame-nq cargo install ripgrep --features pcre2
        have bat
        or hame-nq cargo install bat
        have broot
        or hame-nq cargo install broot
        have starship
        or hame-nq cargo install starship
    end

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

end

