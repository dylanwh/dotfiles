function hame-debian
    test -f /etc/sudoers.d/99-apt
    or echo "$USER ALL = (root) NOPASSWD: /usr/bin/apt, /usr/bin/apt-get" | sudo tee /etc/sudoers.d/99-apt

    set packages          \
        autoconf          \
        build-essential   \
        curl              \
        emacs-nox         \
        fd-find           \
        git               \
        gron              \
        hub               \
        jq                \
        jsonnet           \
        libncurses-dev    \
        libssl-dev        \
        moreutils         \
        ncdu              \
        nmap              \
        nq                \
        pkg-config        \
        protobuf-compiler \
        pv                \
        ripgrep           \
        tmux              \
        vim-nox

    for pkg in $packages
        echo $pkg
    end | sort > ~/.cache/hame/packages.txt

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 ^/dev/null >/dev/null
    or sudo apt install -y $packages
    md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 ^/dev/null

    [ -f /usr/lib/cargo/bin/fd ]
    and not [ -f ~/.local/bin/fd ]
    and ln -sv /usr/lib/cargo/bin/fd ~/.local/bin/fd
end
