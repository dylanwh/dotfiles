function jg --argument term
    set -l fzy_args
    test -n "$term"
    and set fzy_args -q $term

    set dir (fd -Htd .git ~/Code | sed 's/\.git$//' | fzy $fzy_args)
    and cd $dir
    return
end
