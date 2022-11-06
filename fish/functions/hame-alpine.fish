function hame-alpine
    set packages             \
        '!doas-sudo-shim'    \
        '!emacs'             \
        '!emacs-nox'         \
        '!vim'               \
        bash                 \
        bat                  \
        bind-tools           \
        build-base           \
        cmatrix              \
        command-not-found    \
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
        imagemagick          \
        iperf3               \
        jq                   \
        jsonnet              \
        make                 \
        moreutils            \
        mutt                 \
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
        rclone               \
        ripgrep              \
        rsync                \
        skim                 \
        skim-tmux            \
        starship             \
        sudo                 \
        tidyhtml             \
        tmux                 \
        trash-cli            \
        tzdata               \
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
