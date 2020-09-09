function gd
set dir (list-github-repos | sk -p 'cd> ')
and cd ~/Git/$dir
end
