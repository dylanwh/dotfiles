#!/usr/bin/env zsh
# Dylan William Hardison\'s .xinitrc file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ -z $REALNAME ]] && exec zsh -l $0

exec xmonad

# Third re-exec: 
# Ensure wrappers (ssh-agent and dbus) are running, and that we are a login shell.
if [[ -z $DID_WRAPPERS ]]
then
	cmd=(zsh $0)

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
xset r rate 250 30

# 01:16 <jcristau> setxkbmap -option terminate:ctrl_alt_bksp might help
# 01:16 <jcristau> dontzap was turned off again by default in the server, and terminate was made an option in xkeyboard-config
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps
make .Xdefaults

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

#xfce4-panel &
#xfdesktop &
#exec fusion-icon
xmonad | xstatus
