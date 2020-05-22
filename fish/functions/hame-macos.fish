function hame-macos

    if not defaults read com.apple.screencapture location |grep -q Screenshots
        defaults write com.apple.screencapture location ~/Documents/Screenshots
        killall SystemUIServer
    end

    command find ~/.config/alfred -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine
    command find ~/Sync -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine

    test -f /etc/sudoers.d/99-port
    or echo 'dylan ALL = (root) NOPASSWD: /opt/local/bin/port' | sudo tee /etc/sudoers.d/99-port
end
