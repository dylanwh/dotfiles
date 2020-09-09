function hame-macos

    if not defaults read com.apple.screencapture location |grep -q Screenshots
        defaults write com.apple.screencapture location ~/Documents/Screenshots
        killall SystemUIServer
    end
    defaults write com.apple.dock autohide-time-modifier -float 0.15
    killall Dock

    command find ~/.config/alfred -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine
    command find ~/Sync -type f -xattrname com.apple.quarantine -print0 | xargs -0 xattr -vd com.apple.quarantine

    test -f /etc/sudoers.d/99-port
    or echo 'dylan ALL = (root) NOPASSWD: /opt/local/bin/port' | sudo tee /etc/sudoers.d/99-port

    set REPO (realpath (readlink $HOME/.config/fish)/..)

    set st_plist $HOME/Library/LaunchAgents/syncthing.plist
    if not test -f $st_plist
        set st_temp (mktemp)
        mojo-pp $REPO/syncthing.plist.ep > $st_temp
        and command mv -v $st_temp $st_plist
    end
end
