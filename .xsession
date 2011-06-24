#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# First re-exec:
# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Second re-exec:
# Ensure that this script evaluated as a login shell.
[[ -z $REALNAME ]] && exec zsh -l $0

make .Xdefaults &> .Xdefaults.log
xmodmap -e 'keysym Menu = Super_R'
xrdb -merge .Xdefaults
rm -f ~/.panzen

export OOO_FORCE_DESKTOP=gnome
export WINDOW_MANAGER=xmonad

exec startlxde
