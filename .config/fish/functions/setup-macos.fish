function setup-macos
    defaults write com.apple.screencapture location ~/Documents/Screenshots
    and killall SystemUIServer
end