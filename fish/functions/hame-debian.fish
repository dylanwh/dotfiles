function hame-debian
    set packages          \
        autoconf          \
        bind9-dnsutils    \
        black             \
        build-essential   \
        curl              \
        emacs-nox         \
        exa               \
        fd-find           \
        git               \
        gron              \
        httpie            \
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
        vim-nox           \
        whois

    for pkg in $packages
        echo $pkg
    end | sort > ~/.cache/hame/packages.txt

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 2>/dev/null >/dev/null
    or sudo apt install -y $packages
    md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 2>/dev/null

    [ -f /usr/lib/cargo/bin/fd ]
    and not [ -f ~/.local/bin/fd ]
    and ln -sv /usr/lib/cargo/bin/fd ~/.local/bin/fd
end
