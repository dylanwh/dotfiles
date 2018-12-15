function ssh-init
    switch (uname)
    case Linux
        source (ssh-agent -c -a $XDG_RUNTIME_DIR/ssh-agent.socket| sed 's/setenv/set -Ux/g'|psub)
        ssh-add
    case 'darwin'
        ssh-add -K ~/.ssh/id_rsa_2017; ssh-add -K ~/.ssh/id_ed25519_2017;
    end
end
