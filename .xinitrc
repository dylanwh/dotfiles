#!/usr/bin/env zsh
# Dylan William Hardison's .xinitrc file.

# First re-exec:
# Ensure that this script is evaluated by zsh (as a login shell)
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec: ensure wrappers (ssh-agent and dbus) are running.
if [[ -z $DID_WRAPPERS ]]
then
	cmd=(zsh $0)

	# have is ~/bin/have
	if have dbus-launch && [[ -z $DBUS_SESSION_BUS_ADDRESS ]]
	then
		cmd=(dbus-launch --exit-with-session $cmd)
	fi

	if have ssh-agent
	then
		if ! ( [[ -n $SSH_AGENT_PID ]] && kill -0 $SSH_AGENT_PID &> /dev/null )
		then
			cmd=(ssh-agent $cmd)
		fi
	fi
	export DID_WRAPPERS=yes
	exec $cmd
fi
unset cmd DID_WRAPPERS

BGCOLOR='#330000'

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

if [[ -n $SSH_ASKPASS ]]
then
	# for arch linux.
	ssh-add < /dev/null
elif have ssh-askpass
then
	# for debian
	SSH_ASKPASS=ssh-askpass ssh-add < /dev/null
fi

have xmms2-launcher && xmms2-launcher

if [[ $LAPTOP == "yes" ]]
then
	exec xmonad
else
	xmonad | xstatus
fi
