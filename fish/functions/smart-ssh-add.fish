function smart-ssh-add
    set ssh_add_args
    switch $OS
        case Darwin
            set ssh_add_args -- -K
    end

    ssh-add -q -A
    or for file in ~/.ssh/id_ed25519 ~/.ssh/id_rsa
        ssh-add $ssh_add_args $file
    end
end
