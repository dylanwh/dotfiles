#!/usr/bin/env fish
function path -a cmd dir
    if [ -z $cmd ]
        set cmd list
    end
    switch $cmd
        case default
            path add ~/.local/bin
            for env in pyenv plenv
                path add ~/.$env/bin
                path add ~/.$env/shims
            end
            path add $GOPATH/bin
            path add /opt/local/bin
            path add /snap/bin
            path add ~/.cargo/bin
            path add /opt/chefdk/bin
            path add ~/.chefdk/gem/ruby/2.5.0/bin
            path add /opt/chefdk/embedded/bin
            if [ -d $HOME/.wasmtime ]
                set -Ux WASMTIME_HOME "$HOME/.wasmtime"
                path add $WASMTIME_HOME/bin
            end
            path prune
        case add
            if not contains $dir $fish_user_paths
                set -U fish_user_paths $fish_user_paths $dir
            end
        case remove
            set -l new_user_paths
            for path in $fish_user_paths
                if [ $path != $dir ]
                    set new_user_paths $new_user_paths $path
                end
            end
            set -U fish_user_paths $new_user_paths
        case list
            for path in $fish_user_paths
                echo $path
            end
        case empty
            if test (count $fish_user_paths) -eq 0
                return 0
            else
                return 1
            end
        case prune
            set -l new_user_paths
            for path in $fish_user_paths
                if [ -d $path ]
                    set new_user_paths $new_user_paths $path
                end
            end
            set -U fish_user_paths $new_user_paths
        case clear
            set --erase fish_user_paths
        case '*'
            echo "path $cmd: I don't know how"
            return 1
    end
end
