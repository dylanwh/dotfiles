function hame-macos
    if not defaults read com.apple.screencapture location | grep -q Screenshots
        hame-echo adjusting screenshot location
        defaults write com.apple.screencapture location ~/Documents/Screenshots
        killall SystemUIServer
    end

    if not test (defaults read com.apple.dock autohide-time-modifier) -eq 0.15
        hame-echo adjusting dock autohide
        defaults write com.apple.dock autohide-time-modifier -float 0.15
        killall Dock
    end

    defaults write com.microsoft.VSCode \
        ApplePressAndHoldEnabled -bool false
    defaults write com.valvesoftware.steamlink \
        ApplePressAndHoldEnabled -bool false
    defaults write -g AppleShowAllExtensions -bool true

    pushd ~/.config/alfred
    hame-echo updating "~/.config/alfred"
    git pull -q
    popd
    command find ~/.config/alfred -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine

    if not test -f /etc/sudoers.d/99-port
        hame-echo 'allowing passwordless "sudo port"'
        echo 'dylan ALL = (root) NOPASSWD: /opt/local/bin/port' | sudo tee /etc/sudoers.d/99-port
    end

    set macports        \
        autoconf        \
        automake        \
        bat             \
        binplist        \
        coreutils       \
        curlie          \
        fd              \
        findutils       \
        git             \
        go              \
        gron            \
        hey             \
        htop            \
        hub             \
        jq              \
        less            \
        man             \
        moreutils       \
        ncdu            \
        protobuf-c      \
        pstree          \
        pv              \
        py310-black      \
        py310-jupyterlab \
        py310-numpy      \
        py310-openpyxl   \
        py310-pandas     \
        ripgrep         \
        skim            \
        socat           \
        tmux            \
        tmux-pasteboard \
        trash           \
        vim             \
        wget            \
        xml2            \
        xsv             \
        youtube-dl

    for macport in $macports
        echo $macport
    end | sort > ~/.cache/hame/macports.txt

    [ -f ~/.cache/hame/macports.md5 ]
    and cmp -s ~/.cache/hame/macports.md5 (md5 -r ~/.cache/hame/macports.txt|psub)
    or sudo port -N install $macports
    md5 -r ~/.cache/hame/macports.txt > ~/.cache/hame/macports.md5 2>/dev/null
end
