function jd --argument dir
    set -l fzy_args
    if [ -n "$dir" ]
        set fzy_args -q $dir
    end
    set dir (begin
        echo ".."
        fd -Htd
    end | fzy $fzy_args)
    and cd $dir
    return
end
