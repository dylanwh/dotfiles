#!/usr/bin/env zsh
# Dylan William Hardison's .xinitrc file.
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" == "x" ] && exec zsh $0

cd $HOME || exit 1

if [[ -z $DBUS_SESSION_BUS_ADDRESS ]]; then
	if have dbus-launch; then
		exec dbus-launch --exit-with-session zsh $0
	fi
fi

source $SSH_AGENT_FILE

xsetroot -solid '#330000'
xsetroot -cursor_name left_ptr
xrdb -merge $HOME/.Xdefaults.in
xrdb -n $HOME/.Xdefaults.in > .Xdefaults
xmodmap $HOME/.xmodmap

osdd &
rem -z -k'popup remind %s &' &

#socat -b 1024 PIPE:$HOME/.xmonad/status STDOUT | xstatus &
#echo $! > $HOME/.xmonad/xstatus.pid
xmonad | xstatus
