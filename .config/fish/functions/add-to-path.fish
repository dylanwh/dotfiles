function add-to-path
    for path in $argv
        if not contains $path $fish_user_paths
            set -U fish_user_paths $path $fish_user_paths
        end
    end
end
