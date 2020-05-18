function hame
    argparse "f/force" -- $argv
    cd $HOME

    set -lx HAME_FLAGS ""
    if [ $_flag_force ]
        set HAME_FLAGS "--force"
    end

    if not have mosh
        echo installing mosh
        install_mosh
    end
    if not have nq
        echo installing nq
        install_nq
    end

    hame-env
    hame-vim

    if have cargo
        path add ~/.cargo/bin

        have exa
        or hame-nq cargo install exa
        have vivid
        or hame-nq cargo install vivid
        have rg
        or hame-nq cargo install ripgrep
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
end

