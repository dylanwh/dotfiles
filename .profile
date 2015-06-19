REALNAME="Dylan William Hardison"
EMAIL="dylan@hardison.net"
EDITOR="vim"
VISUAL="$EDITOR"
BROWSER="firefox"
MANPAGER='less -s'
PWSAFE_DATABASE="$HOME/annex/private/pwsafe.dat"

export REALNAME EMAIL EDITOR VISUAL BROWSER MANPAGER PWSAFE_DATABASE

LC_COLLATE=POSIX # sort in POSIX order.
TZ=America/New_York

export LC_COLLATE TZ

XDG_DATA_HOME=$HOME/.local/share
XDG_CONFIG_HOME=$HOME/.config
XDG_CACHE_HOME=$HOME/.cache

if [[ -z $XDG_RUNTIME_DIR ]]; then
    XDG_RUNTIME_DIR=/tmp/$LOGNAME
    [[ -d $XDG_RUNTIME_DIR ]] || mkdir $XDG_RUNTIME_DIR
fi

export XDG_DATA_HOME XDG_CONFIG_HOME XDG_CACHE_HOME XDG_RUNTIME_DIR

PATH="$HOME/bin:$PATH"

export PATH
