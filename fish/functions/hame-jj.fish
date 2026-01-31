function hame-jj
    argparse v/verbose -- $argv

    set -lx HAME_VERBOSE $HAME_VERBOSE
    if [ -n "$_flag_verbose" ]
        set HAME_VERBOSE 1
    end

    if have jj
        hame-echo configuring jj
        jj config set --user user.name (git config get user.name)
        jj config set --user user.email (git config get user.email)
    end
end
