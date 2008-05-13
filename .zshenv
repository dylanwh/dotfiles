# Dylan William Hardison's .zshenv file.
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

export REALNAME="Dylan William Hardison"
export EMAIL="dylan@hardison.net"
export EDITOR="vim"
export VISUAL=$EDITOR
export BROWSER="w3m"
export MANPAGER=$HOME/bin/vimpager

export TEMPDIR=~/tmp
export TMPDIR=~/tmp
export HOST=$HOST
export LC_COLLATE=POSIX

export LESSHISTFILE='-'
export SSH_AGENT_FILE=$HOME/.ssh/agent
export LEDGER_FILE=$HOME/pim/ledger
export REMIND_FILE=$HOME/pim/reminders

case $HOST in
	mani) EMAIL="dylan@r-stream.com" ;;
esac

# Do not load any config files from /etc.
setopt noglobalrcs

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
