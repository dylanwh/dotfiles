function hame-debian
    test -f /etc/sudoers.d/99-apt
    or echo "$USER ALL = (root) NOPASSWD: /usr/bin/apt, /usr/bin/apt-get" | sudo tee /etc/sudoers.d/99-apt

    set packages \
        build-essential \
        libssl-dev \
        nq \
        tmux \
        git \
        hub \
        autoconf \
        protobuf-compiler \
        emacs-nox

    for pkg in $packages
        echo $pkg
    end | sort > ~/.cache/hame/packages.txt

    [ -f ~/.cache/hame/packages.md5 ]
    and md5sum -c ~/.cache/hame/packages.md5 ^/dev/null >/dev/null
    or sudo apt install -y $packages
    md5sum ~/.cache/hame/packages.txt > ~/.cache/hame/packages.md5 ^/dev/null
end
