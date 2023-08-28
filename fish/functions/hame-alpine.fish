function hame-alpine
    set -l cache_package_set $HOME/.cache/hame/alpine_package_set.fish
    # detect if / is smaller than 2GB, and if so we use a smaller set of packages
    # since this is unlikely to change, we cache the result
    if [ -f $cache_package_set ]
        source $cache_package_set
    else
        set root_size (df -m / | awk 'NR==2 {print $2}')
        if test $root_size -lt 2048
            echo "set package_set small" > $cache_package_set
        else
            echo "set package_set large" > $cache_package_set
        end
        source $cache_package_set
    end

    hame-echo "using package set $package_set"

    set packages_small \
        bat                  \
        cmatrix              \
        command-not-found    \
        curl                 \
        exa                  \
        fd                   \
        gcc                  \
        git                  \
        nq                   \
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

    set packages_large       \
        '!doas-sudo-shim'    \
        '!emacs'             \
        '!emacs-nox'         \
        '!vim'               \
        $packages_small      \
        bash                 \
        bind-tools           \
        build-base           \
        coreutils            \
        delta                \
        dust                 \
        emacs-x11-nativecomp \
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
        openssl              \
        perl                 \
        perl-mojolicious     \
        procps               \
        xsv

    eval 'set packages $packages_'$package_set

    string join \n $packages |  sort > ~/.cache/hame/packages.txt

    set sudo "sudo"
    if not have sudo
        set sudo doas
    end

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 2>/dev/null >/dev/null
    or $sudo /sbin/apk add $packages
    and md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 2>/dev/null
end
