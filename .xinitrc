#!/usr/bin/env zsh
# Dylan William Hardison's .xinitrc file.
# Ensure that this script is evaluated by zsh
[[ -z $ZSH_NAME ]] && exec zsh $0

source $SSH_AGENT_FILE

cd $HOME || exit 1

xsetroot -solid '#330000'
xsetroot -cursor_name left_ptr

for image in $images; do
	[[ -f $image ]] && feh --bg-center $image
done

xrdb -merge $HOME/.Xdefaults.in
xrdb -n $HOME/.Xdefaults.in > .Xdefaults
xmodmap $HOME/.xmodmap
osdd &
#remind -z -k'gxmessage -title "reminder" %s &' ~/.reminders &

xmonad | socat -b 2048 STDIN PIPE:$HOME/.xmonad/pipe
