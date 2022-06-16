function hame-nq
    if [ -n "$HAME_FOREGROUND" ]
        hame-echo "$argv"
        command $argv
    else
        hame-echo "nq $argv"
        env NQDIR=$HOME/.cache/hame nq -c -q $argv;
    end
end
