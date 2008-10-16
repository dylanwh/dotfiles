#!/usr/bin/env zsh
# Dylan William Hardison's .xinitrc file.
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" == "x" ] && exec zsh $0

BGCOLOR='#330000'

cd $HOME || exit 1

xsetroot -solid $BGCOLOR
xsetroot -cursor_name left_ptr
xrdb -merge $HOME/.Xdefaults.in
xrdb -n $HOME/.Xdefaults.in > .Xdefaults
xmodmap $HOME/.xmodmap

if [[ -z $DBUS_SESSION_BUS_ADDRESS ]]; then
	if have dbus-launch; then
		exec dbus-launch --exit-with-session zsh $0
	fi
fi

urxvt -e x-sshbegin
source $SSH_AGENT_FILE

if have xmms2-launcher; then
	xmms2-launcher
fi

xbindkeys
osdd &
rem -z -k'popup remind %s &' &

while true; do
	xmonad | xstatus
done
