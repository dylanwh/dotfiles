#!/usr/bin/env fish
function path -a cmd
    set -l dirs $argv[2..-1]
    if [ -z $cmd ]
        echo $fish_user_paths
        return
    end
    switch $cmd
        case add
            for path in $dirs
                if not contains $path $fish_user_paths
                    set -U fish_user_paths $path $fish_user_paths
                end
            end
        case '*'
            echo "path $cmd: I don't know how"
            return 1
    end
end
