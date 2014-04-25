#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

xset +fp /usr/share/fonts/local
xset fp rehash
xset r rate 200 35

xsetroot -cursor_name left_ptr

make dirs
xrdb-tt -load

setxkbmap -option
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps

if have synclient; then
    synclient TouchpadOff=1
fi

if have keychain; then
    keys=( $( egrep -l -- '^-{5}BEGIN .* PRIVATE KEY-{5}$' .ssh/*(.) ) )
    eval $(keychain --eval $keys)
fi

source ~/.fehbg

start-pulseaudio-x11

lo current
sleep .5

i3 &> /dev/null
