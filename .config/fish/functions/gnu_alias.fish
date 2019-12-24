function gnu_alias -a cmd
    if test -n "$GNU_PATH"
        for dir in $GNU_PATH
            if test -x "$dir/$cmd"
                alias "$cmd=$dir/$cmd $argv[2..-1]"
                return 0
            end
        end
    end
    alias "$cmd=$cmd $argv[2..-1]"
    return 1
end
