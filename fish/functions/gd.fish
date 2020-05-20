function gd -a dir
    set -l fzy_args
    if [ -n "$dir" ]
        set fzy_args -q $dir
    end
    cd (fd -Htd .git ~/Code | sed 's/\.git$//' | fzy $fzy_args)
end
