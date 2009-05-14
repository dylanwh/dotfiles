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

export TMPDIR=$HOME/tmp
export LC_COLLATE=POSIX # sort in POSIX order.

export LESSHISTFILE='-'
export SSH_AGENT_FILE=$HOME/.ssh/agent@$HOST
export RXVT_SOCKET=$TMPDIR/rxvt-unicode
export LEDGER_FILE=$HOME/pim/ledger
export REMIND_FILE=$HOME/pim/reminders
export YABOOK_FILE=$HOME/pim/contacts.yml
export TODO_FILE=$HOME/pim/todo.xml

export PERL_USE_MOOSE=1

perl5lib=(~/lib 'lib')
path=(~/bin $path)

case $HOST in
	lofn) 
		# to enable UTF-8.
		export LANG=en_US.UTF-8
	;; 
esac

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
