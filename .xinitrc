#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

xset +fp /usr/share/fonts/local
xset fp rehash

if ! which xrdb-tt &>/dev/null; then
	xmessage -center "environment is broken"
	env > ~/BROKEN-ENV.sh
fi

xrdb-tt -load
make
xset r rate 200 35

setxkbmap -option
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps

case $HOST in
	odin)
		syndaemon -k -i 1 -d 
	;;
esac

if have xcalib && [[ -f $XDG_CONFIG_HOME/xcalib/$HOST.icm ]]; then
	xcalib $XDG_CONFIG_HOME/xcalib/$HOST.icm
fi

if have keychain; then
	keys=( $( egrep -l -- '^-{5}BEGIN .* PRIVATE KEY-{5}$' .ssh/*(.) ) )

	eval $(keychain --eval $keys)
fi

source ~/.fehbg

start-pulseaudio-x11 

lo default
sleep .5

i3
