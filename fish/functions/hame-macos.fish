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
end
