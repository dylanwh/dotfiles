#!/bin/bash
shopt -s nullglob
host=$1

find_socket() {
    socket=""
    for s in "$HOME"/.kitty.sock-*; do
        socket="$s"
    done
}

find_socket

if [[ -z "$socket" ]]; then
    open -a kitty
    for _ in {1..300}; do
        sleep 0.1
        find_socket
        [[ -n "$socket" ]] && break
    done
fi

if [[ -n "$ssh_args" ]]; then
    ssh_args=".local/bin/with-nix-env $ssh_args"
    t="-t"
else
    t=""
fi

/Applications/kitty.app/Contents/MacOS/kitty @ --to="unix:$socket" launch --type=tab ssh $t "$host" $ssh_args
