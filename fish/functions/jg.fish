function jg --argument term
    set -l fzy_args
    test -n "$term"
    and set fzy_args -q $term

    pushd ~/Git
    set dir (fd -Htd .git | sed 's/\.git$//' | fzy $fzy_args)
    and cd $dir
    or popd
    return
end
