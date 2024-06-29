function hame-debian
    set packages          \
        autoconf          \
        bind9-dnsutils    \
        black             \
        build-essential   \
        curl              \
        emacs-nox         \
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
        neovim-           \
        nmap              \
        nq                \
        pkg-config        \
        protobuf-compiler \
        pv                \
        ripgrep           \
        tmux              \
        vim-nox-          \
        whois

    set release (lsb_release -rs)
    switch $release
    case 20.04
        set packages (string join \n $packages | egrep -v 'emacs-nox|neovim')
        set -a packages nodejs- emacs-nox- neovim-
    end

    string join \n $packages | sort > ~/.cache/hame/packages.txt

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 2>/dev/null >/dev/null
    or sudo apt install -y $packages
    and md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 2>/dev/null

    [ -f /usr/lib/cargo/bin/fd ]
    and not [ -f ~/.local/bin/fd ]
    and ln -sv /usr/lib/cargo/bin/fd ~/.local/bin/fd
end
