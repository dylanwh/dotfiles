function ssh-adopt -a cmd
    set -q XDG_RUNTIME_DIR
    or set -l XDG_RUNTIME_DIR /tmp
    set -l socket $XDG_RUNTIME_DIR/ssh-agent.socket
    set -l state $XDG_RUNTIME_DIR/ssh-agent.fish

    if test -n "$SSH_AUTH_SOCK"
        test $SSH_AUTH_SOCK = $socket
        and return 1

        set -gx ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
        set -gx SSH_AUTH_SOCK $socket
        chmod go-wrx $ORIGINAL_SSH_AUTH_SOCK

        if test -L "$SSH_AUTH_SOCK"
            set target (readlink $SSH_AUTH_SOCK)
            test $target = $ORIGINAL_SSH_AUTH_SOCK
            and return 0
        else if test -S "$SSH_AUTH_SOCK"
            return 0
        else
            ln -fs $ORIGINAL_SSH_AUTH_SOCK $SSH_AUTH_SOCK
        end
    else if test -e $socket
        set -gx SSH_AUTH_SOCK $socket
    end
end
