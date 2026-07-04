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

kitten=/Applications/kitty.app/Contents/MacOS/kitten
kitty=/Applications/kitty.app/Contents/MacOS/kitty

$kitty @ --to="unix:$socket" launch --type=tab $kitten ssh "$host"
