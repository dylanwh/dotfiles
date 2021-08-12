# Defined in /var/folders/ch/9q4qpbxn3yxczpxmqzpb4xjw0000gn/T//fish.j1mDwB/home.fish @ line 2
function home --wraps='cd ~/Git/dylanwh/home' --wraps='cd ~/Git/dylanwh/dotfiles' --description 'alias home cd ~/Git/dylanwh/dotfiles'
    cd ~/Git/dylanwh/dotfiles
    if [ -n "$argv" ]
        $argv
    end
end
