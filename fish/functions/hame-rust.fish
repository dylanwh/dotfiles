function hame-rust
    cd $HOME

    if not [ -f .cargo/bin/rustup ]
        set rustup_init (mktemp)
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > $rustup_init
        hame-nq sh $rustup_init -y --no-modify-path
        hame-nq rm $rustup_init
    end
end

