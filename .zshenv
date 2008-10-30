# Dylan William Hardison's .zshenv file.
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

export REALNAME="Dylan William Hardison"
export EMAIL="dylan@hardison.net"
export EDITOR="vim"
export VISUAL=$EDITOR
export BROWSER="w3m"
export MANPAGER=less
export TEMPDIR=/tmp
export TMPDIR=/tmp
export LESSHISTFILE='-'
export SSH_AGENT_FILE=$HOME/.ssh/agent
export LEDGER_FILE=$HOME/pim/ledger
export REMIND_FILE=$HOME/pim/reminders
export YABOOK_FILE=$HOME/pim/contacts
export TODO_FILE=$HOME/pim/todo.xml
export LC_COLLATE=POSIX # sort in POSIX order.

export OS_NAME
if [[ -z $OS_NAME ]]; then
	OS_NAME=$(uname -s)
fi

export HOST
if [[ -z $HOST || $HOST =~ "\\." ]]; then
	HOST=$(hostname -s)
fi

case $HOST in
	mani) EMAIL="dylan@r-stream.com" ;;
	lofn) export LANG=en_US.UTF-8 ;;
esac

# Do not load any config files from /etc.
setopt noglobalrcs

declare -gxT PERL5LIB perl5lib
declare -U path cdpath fpath manpath perl5lib

perl5lib=(~/lib 'lib')
path=(~/bin $path)

function have { which $1 &>/dev/null }

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
