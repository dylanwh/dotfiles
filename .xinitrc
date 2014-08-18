#!/usr/bin/env zsh
# Dylan William Hardison\'s .xsession file.

# Ensure that this script is evaluated by zsh
[ "x$ZSH_NAME" = "x" ] && exec zsh -l $0

# Ensure that this script evaluated as a login shell.
[[ ! -o login ]] && exec zsh -l $0

xset +fp /usr/share/fonts/local
xset fp rehash
xset r rate 200 35

xsetroot -cursor_name left_ptr

make dirs
xrdb-tt -load

setxkbmap -option
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option ctrl:nocaps

for rc in /etc/X11/xinit/xinitrc.d/*(.); do
    source $rc
done

eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO SSH_AUTH_SOCK

[[ -f ~/.fehbg ]] && source ~/.fehbg

start-pulseaudio-x11

lo current
sleep .5

i3 &> /dev/null
