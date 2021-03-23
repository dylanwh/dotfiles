function ssh-init -a cmd
    set -q XDG_RUNTIME_DIR
    or set -l XDG_RUNTIME_DIR /tmp
    set -l socket $XDG_RUNTIME_DIR/ssh-agent.socket
    set -l state $XDG_RUNTIME_DIR/ssh-agent.fish

    switch $cmd
        case adopt
            if test -n $SSH_AUTH_SOCK
                if test "$SSH_AUTH_SOCK" = "$socket"
                    echo "unable to adopt self" >&2
                    return 1
                else if test -L "$socket"
                    set old_socket (readlink $socket)
                    if not test $old_socket = $SSH_AUTH_SOCK
                        echo "orphaned $old_socket" >&2
                        rm $socket
                    end
                else if test -S "$socket"
                    set --erase -U SSH_AUTH_SOCK
                    set -gx SSH_AUTH_SOCK $socket
                    return 0
                end

                ln -fs $SSH_AUTH_SOCK $socket
                set -gx SSH_AUTH_SOCK $socket
            end
    end
end
