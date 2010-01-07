#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ -z $REALNAME ]] && exec zsh -l $0

bgcolor='#330000'
delay=200
rate=40

# 01:16 <jcristau> setxkbmap -option terminate:ctrl_alt_bksp might help
# 01:16 <jcristau> dontzap was turned off again by default in the server, and terminate was made an option in xkeyboard-config
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps
make .Xdefaults &> .Xdefaults.log

xsetroot -solid $bgcolor
xsetroot -cursor_name left_ptr

xset r rate $delay $rate

gconftool --type int    --set /desktop/gnome/peripherals/keyboard/rate $rate
gconftool --type int    --set /desktop/gnome/peripherals/keyboard/delay $delay   
gconftool --type string --set /desktop/gnome/applications/window_manager xmonad

#gconftool --type boolean --set /apps/nautilus/preferences/show_desktop false

rm -f ~/.panzen

export WINDOW_MANAGER=xmonad
exec gnome-session
