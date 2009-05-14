#!/usr/bin/env zsh
# Dylan William Hardison's .xinitrc file.
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh $0

BGCOLOR='#330000'

function have { which $1 &>/dev/null }

cd $HOME || exit 1
[[ -d $HOME/tmp ]] || mkdir -m 700 $HOME/tmp

xsetroot -solid $BGCOLOR
xsetroot -cursor_name left_ptr
xrdb -DHOME=$HOME -merge $HOME/.Xdefaults.in
xrdb -DHOME=$HOME -n $HOME/.Xdefaults.in > .Xdefaults
setxkbmap -option ctrl:nocaps

export LAPTOP
case $HOST in
	nott) LAPTOP=yes ;;
	*) LAPTOP=no ;;
esac

if [[ -z $DBUS_SESSION_BUS_ADDRESS ]]
then
	have dbus-launch && exec dbus-launch --exit-with-session zsh $0
fi

if ! ( [[ -n $SSH_AGENT_PID ]] && kill -0 $SSH_AGENT_PID &> /dev/null )
then
	exec ssh-agent zsh $0
fi

# for arch linux.
if [[ -n $SSH_ASKPASS ]]; then
	ssh-add < /dev/null
elif have ssh-askpass; then
	SSH_ASKPASS=$prog ssh-add < /dev/null
fi

have xmms2-launcher && xmms2-launcher

if [[ $LAPTOP == "yes" ]]; then
	exec xmonad
else
	xmonad | xstatus
fi
