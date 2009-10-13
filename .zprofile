# Dylan William Hardison's .zprofile file.
# This script is executed for login shells and before ~/.zshrc for interactive shells.
# See also: ~/.zshenv [~/.zprofile] ~/.zshrc ~/.zlogin ~/.zlogout

export REALNAME="Dylan William Hardison"
export EMAIL="dylan@hardison.net"
export EDITOR="vim"
export VISUAL=$EDITOR
export BROWSER="w3m"
export MANPAGER=less

export HOST=${HOST/.*/}
export OSTYPE=$OSTYPE

export LC_COLLATE=POSIX # sort in POSIX order.
export TZ=US/Eastern

export LESSHISTFILE='-'
export SSH_AGENT_FILE=$HOME/.ssh/agent@$HOST
export PERL_USE_MOOSE=1

declare -U path perl5lib
declare -gxT PERL5LIB perl5lib
perl5lib=(~/lib 'lib')
path=(~/bin $path)

case $HOST in
	lofn) 
		# to enable UTF-8.
		export LANG=en_US.UTF-8
	;; 
esac

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
