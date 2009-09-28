# Dylan William Hardison's .zshrc file. #'
# This script is executed for every interactive shell.
# See also: ~/.zshenv ~/.zprofile [~/.zshrc] ~/.zlogin ~/.zlogout

## {{{ VARIABLES
HISTSIZE=3000
SAVEHIST=3000
HISTFILE=~/.zhistory
READNULLCMD=${PAGER:-/usr/bin/pager}
LOGCHECK=30
watch=(all)
fignore=(.o .hi .pyc)
cdpath=(~ ~/code)
fpath=(~/.zsh $fpath)

## }}}
## {{{ OPTIONS
setopt autocd                  # change to dirs without cd
setopt autopushd               # automatically append dirs to the push/pop list
setopt pushdignoredups         # and do not duplicate them
setopt nocdablevars            # the need for an explicit $
setopt listpacked              # compact completion lists
setopt nolisttypes             # show types in completion
setopt extended_glob           # weird & wacky pattern matching - yay zsh!
setopt alwaystoend             # when complete from middle, move cursor
setopt completeinword          # not just at the end
setopt glob_complete           # complete globs with a menu.
setopt nocorrect               # no spelling correction
setopt promptcr                # add \n which overwrites cmds with no \n
setopt histverify              # when using ! cmds, confirm first
setopt interactivecomments     # escape commands so i can use them later
setopt printexitvalue          # alert me if something has failed
setopt hist_ignore_dups        # ignore same commands run twice+
setopt appendhistory           # do not overwrite history 
setopt nomatch                 # #fooo!
setopt noclobber               # do not overwrite files with >
setopt sharehistory            # share history between all running instances.
setopt hist_find_no_dups       # ignore dups in history search.
setopt noflow_control          # disable control-q/control-s
setopt hashcmds                # avoid having to type 'rehash' all the time.
setopt rm_star_wait            # wait beforing ask if I want to delete all those files...
setopt multios                 # avoid having to use 'tee'
setopt checkjobs               # warn me about bg processes when exiting
setopt nohup                   # and do not kill them, either
setopt auto_continue           # automatically continue disowned jobs.
setopt auto_resume             # automatically resume jobs from commands
## }}}
## {{{ KEY BINDINGS
bindkey -v

case $TERM in
	linux|screen)
		bindkey "^[[1~" beginning-of-line
		bindkey "^[[3~" delete-char
		bindkey "^[[4~" end-of-line
		bindkey "^[[5~" up-line-or-history   # PageUp
		bindkey "^[[6~" down-line-or-history # PageDown
		bindkey "^[[A"  up-line-or-search    # up arrow for back-history-search
		bindkey "^[[B"  down-line-or-search  # down arrow for fwd-history-search
		bindkey "^?"   backward-delete-char
		bindkey "^H"   backward-delete-char
	;;
	rxvt-unicode)
		bindkey "^[[5~" up-line-or-history # pgup
		bindkey "^[[6~" down-line-or-history # pgdown
		bindkey "^[[7~" beginning-of-line  # home
		bindkey "^[[8~" end-of-line        # end
		bindkey "^[[A" up-line-or-search   # up arrow
		bindkey "^[[B" down-line-or-search # down arrow
		bindkey "^?"   backward-delete-char
		bindkey "^H"   backward-delete-char
	;;
	*xterm*|rxvt*|(dt|k|E)term)
		bindkey "^[[2~" yank
		bindkey "^[[3~" delete-char
		bindkey "^[[5~" up-line-or-history # PageUp
		bindkey "^[[6~" down-line-or-history # PageDown
		bindkey "^[[7~" beginning-of-line
		bindkey "^[[8~" end-of-line
		bindkey "^[[A" up-line-or-search ## up arrow for back-history-search
		bindkey "^[[B" down-line-or-search ## down arrow for fwd-history-search
		bindkey " " magic-space ## do history expansion on space
	;;
esac

bindkey -a q quote-line
bindkey -a Q quote-region
bindkey -a 'H' run-help
bindkey "^_" copy-prev-shell-word
bindkey '^P' push-input
bindkey '^[h' run-help
bindkey '^r' vi-history-search-backward

## }}}
## {{{ FUNCTIONS
function chpwd {
	ztitle
	have todo && todo --timeout --summary 
}
function mdc { mkdr -p $1 && cd $1 }
function shuffle {
	RANDOM=`date +%s`
	(
	while IFS= read -r i; do
	    echo $RANDOM$RANDOM "$i";
	done
	) | sort | sed 's/^[0-9]* //'
}
function namedir {
	declare -g $1=$2
	: ~$1
}
## }}}
## {{{ ALIASES
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias md="mkdir -p"
alias rd="rmdir"
alias df="df -h"
alias free="free -m"
alias grep='egrep --color=auto'
alias ggrep='command grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias ls='ls --color=auto -F -h'
alias la='ls -ax'
alias ll='ls -l'
alias lsd='ls -d *(/)'
alias vi=vim
alias gvi=gvim
alias vimrc="$EDITOR ~/.vimrc"
alias muttrc="$EDITOR ~/.mutt/muttrc"
alias zrc='vim ~/.zshrc'
alias zenv='vim ~/.zshenv' 
alias xrc='vim ~/.xinitrc'
alias xmrc='vim ~/.xmonad/xmonad.hs'
alias xs=cd
alias zreload='exec env SHLVL=0 $SHELL'
alias help=run-help
alias pdoc=perldoc
alias g="sr google"
alias pd=popd
alias pu=pushd
alias find="noglob find"
alias menu="vim ~/.menu"
alias x2=xmms2
alias cnm='cnetworkmanager'
## }}}

# Autoload various functions
autoload sshbegin sshend run-help ztitle
autoload compinit promptinit colors

# initialize advanced tab completion.
compinit -d ~/.zcompdump

colors
promptinit   # Setup prompt theming 
prompt dylan # Set the prompt.

umask  022   # Create files that are read-only by group.
stty -ixon   # Disable the freeze-the-terminal-on-control-s thing.
ttyctl -f    # Freeze terminal properties.

# Add sbin directories for sudo tab completion.
zstyle ':completion:*:sudo:*' command-path $path /usr/sbin /sbin

have pinfo && alias info=pinfo
have ack-grep && alias ack=ack-grep

for dircolors in dircolors gdircolors; do
	if have $dircolors; then
		unset LS_COLORS
		eval $($dircolors ~/.dir_colors)
		# Colorize completions.
		zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
		break
	fi
done

if [[ -d /opt/perl ]]; then
	path=(/opt/perl/bin $path)
fi

case $OSTYPE in
	*bsd*)
		unalias grep egrep fgrep ggrep
		alias ls="ls -Fh"
	;;
esac

ztitle
have todo && todo --timeout --summary

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
