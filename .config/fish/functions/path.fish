#!/usr/bin/env fish
function path -a cmd
    set -l args $argv[2..-1]
    if [ -z $cmd ]
        set cmd list
    end
    switch $cmd
        case add
            for add in $args
                if not contains $add $fish_user_paths
                    set -U fish_user_paths $add $fish_user_paths
                end
            end
        case remove
            set -l new_user_paths
            for path in $fish_user_paths
                if not contains $path $args
                    set new_user_paths $new_user_paths $path
                end
            end
            set -U fish_user_paths $new_user_paths
        case list
            for path in $fish_user_paths
                echo $path
            end
        case prune
            set -l new_user_paths
            for path in $fish_user_paths
                if [ -d $path ]
                    set new_user_paths $new_user_paths $path
                end
            end
            set -U fish_user_paths $new_user_paths
        case '*'
            echo "path $cmd: I don't know how"
            return 1
    end
end
