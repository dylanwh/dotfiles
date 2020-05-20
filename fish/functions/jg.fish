function jg -a term
    pushd ~/Git
    set -l fzy_args 
    if [ -n "$term" ]
        set fzy_args -e $term
    end
    set results (fd -Htd '^\.git$' | sed 's/\.git$//' | fzy $fzy_args)
    set result $results[1]
    if [ (count $results) -gt 1 ]
        if not set result (string collect $results | fzy)
            popd
            return
        end
    end
    test -n "$result"
    and cd $result
end
