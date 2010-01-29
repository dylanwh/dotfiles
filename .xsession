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

gconftool -t list --list-type string \
    -s /desktop/gnome/peripherals/keyboard/kbd/options '[ctrl   ctrl:nocaps,terminate   terminate:ctrl_alt_bksp]'
gconftool -t boolean \
    -s /desktop/gnome/peripherals/keyboard/general/groupPerWindow false
gconftool -t int     -s /desktop/gnome/peripherals/keyboard/rate $rate
gconftool -t int     -s /desktop/gnome/peripherals/keyboard/delay $delay
gconftool -t string  -s /desktop/gnome/applications/window_manager xmonad
gconftool -t boolean -s /apps/nautilus/preferences/show_desktop false
gconftool -t boolean -s /desktop/gnome/background/draw_background false
gconftool -t boolean -s /desktop/gnome/background/disable_background true
gconftool -t string  -s /desktop/gnome/interface/gtk_theme Dust

rm -f ~/.panzen

export WINDOW_MANAGER=xmonad
exec gnome-session
