function hame-rust
    path add ~/.cargo/bin
    if not [ -f .cargo/bin/rustup ]
        set rustup_init (mktemp)
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > $rustup_init
        hame-nq sh $rustup_init -y --no-modify-path
        hame-nq rm $rustup_init
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
        have broot
        and hame-nq cargo uninstall broot
    end
end

