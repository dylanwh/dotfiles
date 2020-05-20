function gnu_alias -a cmd
    set -l gnu_path /usr/local/opt/*/libexec/gnubin
    if test -d /opt/local/libexec/gnubin
        set gnu_path $gnu_path /opt/local/libexec/gnubin
    end
    if test -n "$gnu_path"
        for dir in $gnu_path
            if test -x "$dir/$cmd"
                alias "$cmd=$dir/$cmd $argv[2..-1]"
                return 0
            end
        end
    end
    return 1
end
