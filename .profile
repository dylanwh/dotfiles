REALNAME="Dylan William Hardison"
EMAIL="dylan@hardison.net"
EDITOR="emacsclient"
VISUAL="$EDITOR"
BROWSER="firefox"
MANPAGER='less -s'
PWSAFE_DATABASE="annex/private/pwsafe.dat"

export REALNAME EMAIL EDITOR VISUAL BROWSER MANPAGER PWSAFE_DATABASE

LC_COLLATE=POSIX # sort in POSIX order.
TZ=America/New_York

export LC_COLLATE TZ

XDG_DATA_HOME=$HOME/.local/share
XDG_CONFIG_HOME=$HOME/.config
XDG_CACHE_HOME=$HOME/.cache

export XDG_DATA_HOME XDG_CONFIG_HOME XDG_CACHE_HOME


PATH="/home/dylan/bin:$PATH"

export PATH
