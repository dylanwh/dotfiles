function hame-clone
    argparse 'n/name=' 'p/path=' 'u/url=' -- $argv
    or return 1

    if not [ -n "$_flag_path" -a -n "$_flag_url" -a "$_flag_name" ]
        return 2
    end

    pushd $HOME
    if ! [ -d "$_flag_path/.git" ]
        set -l parent (dirname $_flag_path)
        if [ "$parent" != "." ]
            mkdir -vp $parent
        end
        hame-echo git clone $_flag_name
        command git clone --depth 1 https://github.com/$_flag_url $_flag_path
    end
    popd
end

