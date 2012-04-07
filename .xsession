#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

host=$(hostname -s)
bg_color='#000033'
bg_image="$HOME/Dropbox/Photos/background/$host.jpg"
kb_delay=200
kb_rate=40

xset +fp /usr/share/fonts/local
xset fp rehash

setxkbmap -option terminate:ctrl_alt_bksp \
	      -option altwin:swap_alt_win \
		  -option ctrl:nocaps

xsetroot -solid $bg_color
xsetroot -cursor_name left_ptr
xset r rate $kb_delay $kb_rate

xrdb-reload
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
parcellite &
nm-applet &
be xmonad-log &
be xmonda-tray &

exec dterm
