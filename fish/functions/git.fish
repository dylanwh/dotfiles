function git --description 'alias git=hub'
    if [ "$PWD" = "$HOME" ]
        switch $argv[1]
            case 'clone'
                set name (git-repo-name $argv[2])
                set dir ~/Git/(dirname $name)
                mkdir -p $dir
                set argv "-C" "$dir" $argv
            case '*'
                set argv "-C" ~/Git/dylanwh/home $argv
        end
    end
    hub $argv
end
