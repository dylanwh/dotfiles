# Defined in /var/folders/ch/9q4qpbxn3yxczpxmqzpb4xjw0000gn/T//fish.TfflIJ/gd.fish @ line 1
function gd --argument query
    test -n "$query"
    and set sk_args -q "$query"
    set dir (list-github-repos | sk $sk_args -p 'cd> ')
    and cd ~/Git/$dir
end
