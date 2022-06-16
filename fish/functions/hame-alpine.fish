function hame-alpine
    set packages         \
        bash             \
        bat              \
        bind-tools       \
        build-base       \
        cmatrix          \
        coreutils        \
        curl             \
        doas             \
        doas-sudo-shim   \
        '!sudo'          \
        emacs            \
        exa              \
        fd               \
        gcc              \
        git              \
        go               \
        htop             \
        httpie           \
        iperf3           \
        jq               \
        jsonnet          \
        make             \
        moreutils        \
        ncdu             \
        ncurses          \
        nq               \
        openssl          \
        perl             \
        perl-mojolicious \
        procps           \
        psmisc           \
        pv               \
        rclone           \
        ripgrep          \
        rsync            \
        skim             \
        skim-tmux        \
        starship         \
        tmux             \
        vim              \
        wget

    for pkg in $packages
        echo $pkg
    end | sort > ~/.cache/hame/packages.txt

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 2>/dev/null >/dev/null
    or sudo apk add $packages
    md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 2>/dev/null
end
