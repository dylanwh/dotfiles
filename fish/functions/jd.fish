function jd -a term
    set -l fzy_args
    if [ -n "$term" ]
        set fzy_args -e $term
    end
    set results (begin
        echo ..
        fd -td
    end | fzy $fzy_args)

    set result $results[1]
    if [ (count $results) -gt 1 ]
        if not set result (string collect $results | fzy )
            return
        end
    end
    test -n "$result"
    and cd $result
end
