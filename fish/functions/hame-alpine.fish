function hame-alpine
    set packages             \
        '!doas-sudo-shim'    \
        '!vim'               \
        '!emacs'             \
        '!emacs-nox'         \
        bash                 \
        bat                  \
        bind-tools           \
        build-base           \
        cmatrix              \
        coreutils            \
        curl                 \
        doas                 \
        emacs-x11-nativecomp \
        exa                  \
        fd                   \
        gcc                  \
        git                  \
        go                   \
        htop                 \
        httpie               \
        iperf3               \
        jq                   \
        jsonnet              \
        make                 \
        moreutils            \
        ncdu                 \
        ncurses              \
        neovim               \
        neovim-doc           \
        nodejs               \
        nq                   \
        openssl              \
        perl                 \
        perl-mojolicious     \
        procps               \
        psmisc               \
        pv                   \
        tzdata               \
        rclone               \
        ripgrep              \
        rsync                \
        skim                 \
        skim-tmux            \
        starship             \
        sudo                 \
        tmux                 \
        wget

    string join \n $packages | sort > ~/.cache/hame/packages.txt

    set sudo "sudo"
    if not have sudo
        set sudo doas
    end

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 2>/dev/null >/dev/null
    or $sudo /sbin/apk add $packages
    and md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 2>/dev/null
end
