#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ -z $REALNAME ]] && exec zsh -l $0

BGCOLOR='#330000'

xsetroot -solid $BGCOLOR
xsetroot -cursor_name left_ptr
xset r rate 250 30

# 01:16 <jcristau> setxkbmap -option terminate:ctrl_alt_bksp might help
# 01:16 <jcristau> dontzap was turned off again by default in the server, and terminate was made an option in xkeyboard-config
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps
make .Xdefaults

#gconftool --type boolean --set /apps/nautilus/preferences/show_desktop false

export WINDOW_MANAGER=xmonad
exec gnome-session
