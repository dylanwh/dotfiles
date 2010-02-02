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

# no, do NOT override my xkbmaps!
kb_options="[terminate\tterminate:ctrl_alt_bksp,ctrl\tctrl:nocaps,Compose key\tcompose:menu]"
gconftool -t list --list-type string \
    -s /desktop/gnome/peripherals/keyboard/kbd/options "$(echo -e $kb_options)"
# no, I do not want per-window keymaps!
gconftool -t boolean \
    -s /desktop/gnome/peripherals/keyboard/general/groupPerWindow false
gconftool -t int     -s /desktop/gnome/peripherals/keyboard/rate $rate
gconftool -t int     -s /desktop/gnome/peripherals/keyboard/delay $delay
# yes, xmonad is REALLY my window manager. gnome IGNORES $WINDOW_MANAGER at times.
gconftool -t string  -s /desktop/gnome/applications/window_manager xmonad

# the following lines make the desktop behave as I want.
gconftool -t boolean -s /apps/nautilus/preferences/show_desktop false
gconftool -t boolean -s /desktop/gnome/background/draw_background false
gconftool -t boolean -s /desktop/gnome/background/disable_background true
gconftool -t string  -s /desktop/gnome/interface/gtk_theme Dust
gconftool -t string -s  /desktop/gnome/background/picture_filename ""
gconftool -t string -s  /desktop/gnome/background/primary_color $bgcolor
gconftool -t string -s  /desktop/gnome/background/secondary_color $bgcolor

rm -f ~/.panzen

export WINDOW_MANAGER=xmonad
exec gnome-session
