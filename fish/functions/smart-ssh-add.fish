function smart-ssh-add
    set ssh_add_args
    switch $OS
        case Darwin
            ssh-add -q --apple-load-keychain
            or for file in ~/.ssh/id_ed25519 ~/.ssh/id_rsa
                test -f $file
                and ssh-add --apple-use-keychain $file
            end
        case '*'
            if test -S ~/.1password/agent.sock
                set -Ux SSH_AUTH_SOCK $HOME/.1password/agent.sock
            else
                ssh-add -q
                or for file in ~/.ssh/id_ed25519 ~/.ssh/id_rsa
                    test -f $file
                    and ssh-add $file
                end
            end
    end
end
