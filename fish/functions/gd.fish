# Defined in /var/folders/ch/9q4qpbxn3yxczpxmqzpb4xjw0000gn/T//fish.bHGcgb/gd.fish @ line 2
function gd --argument query
    test -n "$query"
    and set sk_args -q "$query"
    set -lx SKIM_DEFAULT_COMMAND list-github-repos
    set dir (sk-tmux $sk_args )
    and cd ~/Git/$dir
    commandline -f repaint
end
