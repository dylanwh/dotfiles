function smart-ssh-agent
    argparse "f/force" "r/revert" -- $argv

    set -q XDG_RUNTIME_DIR
    or set -l XDG_RUNTIME_DIR /tmp
    set -l socket $XDG_RUNTIME_DIR/ssh-agent.socket
    set -l state $XDG_RUNTIME_DIR/ssh-agent.fish

    if test -n "$_flag_revert" -a -n "$ORIGINAL_SSH_AUTH_SOCK"
        set -gx SSH_AUTH_SOCK "$ORIGINAL_SSH_AUTH_SOCK"
        return
    end

    if test -n "$_flag_force" -a -n "$ORIGINAL_SSH_AUTH_SOCK"
        set -gx SSH_AUTH_SOCK "$ORIGINAL_SSH_AUTH_SOCK"
    end

    if test -S "$socket" -a ! -n "$_flag_force"
        test -n "$SSH_AUTH_SOCK"
        and set -gx ORIGINAL_SSH_AUTH_SOCK "$SSH_AUTH_SOCK"
        set -gx SSH_AUTH_SOCK "$socket"
    else if test -n "$SSH_AUTH_SOCK"
        test "$SSH_AUTH_SOCK" = "$socket"
        and return 1

        set -gx ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
        set -gx SSH_AUTH_SOCK $socket
        chmod go-wrx $ORIGINAL_SSH_AUTH_SOCK
        ln -fs $ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
    else
        test -f $state
        and source $state
        or source (ssh-agent -c | tee $state | psub)
        set -gx ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
        set -gx SSH_AUTH_SOCK $socket
        chmod go-wrx $ORIGINAL_SSH_AUTH_SOCK
        ln -fs $ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
    end
end
