function hame-rust
    user-path add ~/.cargo/bin

    if test -d /nix
        have rustup
        and rustup default stable
        and rustup component add rust-analyzer rust-src rustfmt clippy
        return 0
    else
        if not have rustup
            hame-echo installing rust
            set rustup_init (mktemp)
            curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs >$rustup_init
            sh $rustup_init -y --no-modify-path
            rm $rustup_init

        end
    end

    if have cargo
        have sk
        or hame-nq cargo install skim
        have fd
        or hame-nq cargo install fd-find
        have rg
        or hame-nq cargo install ripgrep --features pcre2
        have exa
        and cargo uninstall exa
        have eza
        or hame-nq cargo install eza
        have bat
        or hame-nq cargo install bat
        have vivid
        or hame-nq cargo install vivid
        have starship
        or hame-nq cargo install starship
        have dust
        or hame-nq cargo install du-dust
        if [ (uname -sm) != "Linux aarch64" ]
            have delta
            or hame-nq cargo install git-delta
        end
        have uq
        or hame-nq cargo install uq
        have xsv
        or hame-nq cargo install xsv
        have shpool
        and hame-nq cargo uninstall shpool
        have mdopen
        or hame-nq cargo install mdopen
    end
end
