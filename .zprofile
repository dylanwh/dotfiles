# Dylan William Hardison's .zprofile file.
# This script is executed for login shells and before ~/.zshrc for interactive shells.
# See also: ~/.zshenv [~/.zprofile] ~/.zshrc ~/.zlogin ~/.zlogout
# #'

export REALNAME="Dylan William Hardison"
export EMAIL="dylan@hardison.net"
export EDITOR="vim"
export VISUAL="$EDITOR"
export BROWSER="chrome"
export MANPAGER='less -s'

export HOST="${HOST/.*/}"
export OSTYPE="$OSTYPE"

export LC_COLLATE=POSIX # sort in POSIX order.
export TZ=US/Eastern

path=(~/bin ~/app/urxvt/bin $path)
perl5lib=(~/lib 'lib')

case $HOST in
    lofn)
        # to enable UTF-8.
        export LANG=en_US.UTF-8
    ;; 
esac

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
