#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

keyboard_delay=200
keyboard_rate=30

xset +fp /usr/share/fonts/local
xset fp rehash
xset r rate $keyboard_delay $keyboard_rate

xrdb-reload
setxkbmap -option terminate:ctrl_alt_bksp \
	      -option altwin:swap_alt_win \
		  -option ctrl:nocaps

source ~/.fehbg


exec i3
