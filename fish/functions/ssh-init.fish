function ssh-init
    switch (uname)
    case Linux
        set -l socket $XDG_RUNTIME_DIR/ssh-agent.socket
        set -l state  $XDG_RUNTIME_DIR/ssh-agent.fish
        set --erase -g SSH_AUTH_SOCK
        set --erase -g SSH_AGENT_PID
        source (ssh-agent -c -a $socket | sed 's/setenv/set -Ux/g' | tee $state | psub)
        ssh-add
    case 'Darwin'
        ssh-add -K ~/.ssh/id_ed25519
        ssh-add -K ~/.ssh/id_rsa
    end
end
