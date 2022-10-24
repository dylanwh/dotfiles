function hame-clone
    argparse 'n/name=' 'p/path=' 'u/url=' 'b/branch=' 'U/update' -- $argv
    or return 1

    if not [ -n "$_flag_path" -a -n "$_flag_url" -a "$_flag_name" ]
        echo "Usage: hame-clone [-n name] [-p path] [-u url] [-b branch] [-U]" 2>&1
        return 2
    end

    pushd $HOME
    if [ -d "$_flag_path/.git" ]
        if [ -n "$_flag_update" ]
            pushd $_flag_path
            hame-echo git pull $_flag_name
            command git pull
            popd
        end
    else
        set -l parent (dirname $_flag_path)
        if [ "$parent" != "." ]
            mkdir -vp $parent
        end
        hame-echo git clone $_flag_name
        set -l git_args --depth 1
        if [ -n "$_flag_branch" ]
            set git_args $git_args --branch=$_flag_branch
        end
        command git clone $git_args https://github.com/$_flag_url $_flag_path
    end
    popd
end

