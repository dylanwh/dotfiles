# Dylan William Hardison's .zshenv file.
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

declare -gxT PERL5LIB perl5lib # declare array
declare -U path perl5lib       # remove duplicates
setopt noglobalrcs             # Do not load any config files from /etc.
(( SHLVL > 1 )) && return 0    # Stop here if subshell.

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
export RXVT_SOCKET="$TMPDIR/rxvt-unicode"
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
