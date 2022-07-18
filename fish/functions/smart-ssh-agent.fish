function smart-ssh-agent
    set -q XDG_RUNTIME_DIR
    or set -l XDG_RUNTIME_DIR /tmp
    set -l socket $XDG_RUNTIME_DIR/ssh-agent.socket
    set -l state $XDG_RUNTIME_DIR/ssh-agent.fish

    test -f $state
    and source $state
    and kill -0 $SSH_AGENT_PID 2>/dev/null
    or source (ssh-agent -c | tee $state | psub)
end
