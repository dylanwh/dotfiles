#!/usr/bin/env zsh
#Ensure that this script is evaluated by zsh
[[ -z $ZSH_NAME ]] && exec zsh $0

source ~/.zprofile
source $SSH_AGENT_FILE

xfont="-xos4-terminus-bold-r-*-*-*-120-100-100-*-*-iso8859-1"
xmodmap=$HOME/.xmodmap
images=('/srv/common/images/backgrounds/Crab_Nebula_1440x900.jpg')

case $(uname -s) in
	Darwin)
		xfont='-*-terminus-bold-*-*-*-13-*-*-*-*-*-iso8859-1'
		xmodmap=$HOME/.xmodmap.osx
		for dir in /usr/lib/X11/fonts/*; do
			xset +fp $dir
		done
		xset fp rehash
		quartz-wm --only-proxy &
	;;
esac

export XFONT=$xfont
export XTERMCMD=pterm

cd $HOME || exit 1

xsetroot -solid '#330000'
xsetroot -cursor_name left_ptr

for image in $images; do
	[[ -f $image ]] && feh --bg-center $image
done

xrdb -DXFONT=$xfont -merge .Xdefaults.in
xrdb -DXFONT=$xfont -n .Xdefaults.in > .Xdefaults
#xmodmap $xmodmap

integer width=$(xdpyinfo|awk '/dimensions:/ {print $2}' | head -1 | cut -dx -f1)
integer irc_width irc_pos mail_width mail_pos bar_width bar_pos

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
	dzen2 -ta r -tw $now_width -x $now_pos  -fn $XFONT -e click=nothing &
checkmail | dzen2      -ta r -w $mail_width  -x $mail_pos -fn $XFONT  -e click=nothing &
#checkirc  | dzen2 -l 6 -ta l -tw $irc_width -x $irc_pos  -fn $XFONT   &

exec xmonad | xmonad-status | dzen2 -ta l -e 'click=nothing' -fn $XFONT -w $bar_width -x $bar_pos
