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

function have { which $1 &>/dev/null }

if [[ -z $DBUS_SESSION_BUS_ADDRESS ]]
then
	if have dbus-launch; then
		exec dbus-launch --exit-with-session zsh $0
	fi
fi

if ! ( [[ -n $SSH_AGENT_PID ]] && kill -0 $SSH_AGENT_PID &> /dev/null )
then
	have ssh-agent && exec ssh-agent zsh $0
fi

have ssh-askpass    && SSH_ASKPASS=ssh-askpass ssh-add < /dev/null
have xmms2-launcher && xmms2-launcher
have xbindkeys      && xbindkeys
have remind         && rem -z -k'popup remind %s &' &
osdd &

if have xmonad; then
	while true; do
		xmonad | xstatus
	done
else
	WM='xterm'
	for wm in wmaker openbox urxvt rxvt; do
		if have wm; then
			WM=$wm
			break
		fi
	done
	exec $WM
fi
