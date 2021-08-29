function hame-rust
    if [ -d /opt/rust ]
        set -Ux CARGO_HOME /opt/rust/cargo
        set -Ux RUSTUP_HOME /opt/rust/rustup
        path add $CARGO_HOME/bin
    else
        path add ~/.cargo/bin
    end

    pushd $HOME
    if not have rustup
        hame-echo installing rust
        set rustup_init (mktemp)
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > $rustup_init
        sh $rustup_init -y --no-modify-path
        rm $rustup_init
    end

    if have cargo
        have sk
        or hame-nq cargo install skim
        have fd
        or hame-nq cargo install fd-find
        have rg
        or hame-nq cargo install ripgrep --features pcre2
        have exa
        or hame-nq cargo install exa
        have bat
        or hame-nq cargo install bat
        have vivid
        or hame-nq cargo install vivid
        have starship
        or hame-nq cargo install starship
        have procs
        or hame-nq cargo install procs
        have dust
        or hame-nq cargo install du-dust
        have delta
        or hame-nq cargo install git-delta
        have xsv
        or hame-nq cargo install xsv
        have broot
        and hame-nq cargo uninstall broot
    end
    popd
end

