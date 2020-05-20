function jd
    argparse "a/all" -- $argv
    set dir $argv[1]
    set -l fd_args "-td"
    set -l fzy_args
    if [ -n "$dir" ]
        set -a fzy_args -q $dir
    end
    if [ $_flag_all ]
        set -a fd_args "-H"
    end
    set dir (begin
        echo ".."
        fd $fd_args
    end | fzy $fzy_args)
    and cd $dir
    return
end
