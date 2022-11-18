function hame-clone
    argparse \
        'n/name=' \
        'p/path=' \
        'u/url=' \
        'b/branch=' \
        'U/update' \
        'h/help' \
        -- $argv
    or return 1

    if test -n "$_flag_help"
        echo "Usage: hame-clone [-n name] [-p path] [-u url] [-b branch] [-U] [-h]"
        echo "  -n name: name of the repository"
        echo "  -p path: path to clone the repository"
        echo "  -u url: url of the repository, relative to github.com"
        echo "  -b branch: branch to checkout"
        echo "  -U: update the repository"
        echo "  -h: show this help"
        return 0
    end

    if not [ -n "$_flag_path" -a -n "$_flag_url" -a "$_flag_name" ]
        echo "Usage: hame-clone [-n name] [-p path] [-u url] [-b branch] [-U]" 2>&1
        return 2
    end

    set -lx HAME_UPDATE $HAME_UPDATE
    if test -n "$_flag_update"
        set HAME_UPDATE yes
    end

    pushd $HOME
    if [ -d "$_flag_path/.git" ]
        if [ -n $HAME_UPDATE ]
            pushd $_flag_path
            hame-echo git pull $_flag_name
            command git pull -q
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

