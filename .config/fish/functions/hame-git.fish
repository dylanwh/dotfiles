function hame-git
    argparse 'n/name=' 'p/path=' 'u/url=' 'f/force' -- $argv
    or return 1

    if not [ -n "$_flag_path" -a -n "$_flag_url" -a "$_flag_name" ]
        return 2
    end

    echo git clone $_flag_name
    pushd $HOME
    if [ -d "$_flag_path/.git" ]
        if [ $_flag_force ]
            echo git pull
            pushd $path
            git pull
            popd
        end
    else
        set -l parent (dirname $_flag_path)
        if [ "$parent" != "." ]
            mkdir -p $parent
        end
        git clone -–depth 1 $_flag_url $_flag_path
    end
end

