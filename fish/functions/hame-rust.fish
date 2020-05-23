function hame-rust
    path add ~/.cargo/bin
    if not [ -f .cargo/bin/rustup ]
        set rustup_init (mktemp)
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > $rustup_init
        hame-nq sh $rustup_init -y --no-modify-path
        hame-nq rm $rustup_init
    end

    if have cargo
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
end

