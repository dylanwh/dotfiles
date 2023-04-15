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

    defaults write com.googlecode.iterm2 PrefsCustomFolder -string ~/.config/iterm2
    defaults write com.googlecode.iterm2 LoadPrefsFromCustomFolder -bool true

    pushd ~/.config/alfred
    hame-echo updating "~/.config/alfred"
    git pull -q
    popd
    command find ~/.config/alfred -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine

    if not test -f /etc/sudoers.d/99-port
        hame-echo 'allowing passwordless "sudo port"'
        echo 'dylan ALL = (root) NOPASSWD: /opt/local/bin/port' | sudo tee /etc/sudoers.d/99-port
    end

    set macports         \
        autoconf         \
        automake         \
        bash             \
        bat              \
        binplist         \
        cmake            \
        coreutils        \
        curlie           \
        dust             \
        emacs +native-comp \
        exa              \
        fd               \
        findutils        \
        git              \
        git-delta        \
        go               \
        gron             \
        hey              \
        htop             \
        hub              \
        iperf3           \
        jq               \
        less             \
        man              \
        moreutils        \
        ncdu1            \
        neovim           \
        nodejs16         \
        protobuf-c       \
        pstree           \
        pv               \
        py310-black      \
        py310-jupyterlab \
        py310-numpy      \
        py310-openpyxl   \
        py310-pandas     \
        ripgrep          \
        shellcheck       \
        skim             \
        socat            \
        tmux             \
        tmux-pasteboard  \
        trash            \
        vim              \
        wget             \
        xml2             \
        xsv              \
        youtube-dl

    string join \n $macports | sort > ~/.cache/hame/macports.txt

    [ -f ~/.cache/hame/macports.md5 ]
    and cmp -s ~/.cache/hame/macports.md5 (md5 -r ~/.cache/hame/macports.txt|psub)
    or sudo port -N install (string split $macports)
    and md5 -r ~/.cache/hame/macports.txt > ~/.cache/hame/macports.md5 2>/dev/null

    sudo port select --set black black310
    sudo port select --set python python310
    sudo port select --set python3 python310
end
