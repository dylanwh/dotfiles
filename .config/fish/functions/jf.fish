function jf --argument term
    set -l fzy_args
    if [ -n "$term" ]
        set fzy_args -q $term
    end
    set file (fd -tf -tl | fzy $fzy_args)
    and $EDITOR $file
    return 0
end
