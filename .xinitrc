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

integer width=$(xdpyinfo|awk '/dimensions:/ {print $2}' | head -1 | cut -dx -f1)
integer irc_width irc_pos mail_width mail_pos bar_width bar_pos
declare xfont=$(awk 'BEGIN { FS = ":[\t ]+" } /Rxvt.font/ {print $2}' .Xdefaults)

# widths
(( bar_width  = (width / 2) + ((width/2) / 2) ))
(( irc_width  = 0 )) # bar_width / 2))
(( now_width  = 56 ))
(( mail_width = width - bar_width - irc_width - now_width ))

(( bar_pos    = 0        ))
(( now_pos    = width - now_width ))
(( irc_pos    = bar_width ))
(( mail_pos   = bar_width + irc_width ))

while true; do date +%H:%M; sleep 30; done | \
	dzen2 -bg black -ta r -tw $now_width -x $now_pos  -fn $xfont -e click=nothing &
checkmail | dzen2 -bg black -ta r -w $mail_width  -x $mail_pos -fn $xfont  -e click=nothing &
#checkirc  | dzen2 -l 6 -ta l -tw $irc_width -x $irc_pos  -fn $xfont   &

exec xmonad | xmonad-status | dzen2 -bg black -ta l -e 'click=nothing' -fn $xfont -w $bar_width -x $bar_pos
