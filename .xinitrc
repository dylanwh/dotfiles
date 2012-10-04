#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

xset +fp /usr/share/fonts/local
xset fp rehash

xrdb-reload

xset r rate 200 35

setxkbmap -option
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps

case $HOST in
	odin)
		dev=$(xinput list --id-only 'AT Translated Set 2 keyboard')
		[[ -n $dev ]] && setxkbmap -device $dev -option altwin:swap_alt_win 
	;;
esac

source ~/.fehbg

exec i3
