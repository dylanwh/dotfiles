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
cdpath=(~ ~/.local/Dropbox ~/code)
fpath=(~/.zsh/lib $fpath)

export PS_PERSONALITY=linux
export XMMS_PATH='tcp://:1985'

## }}}
## {{{ OPTIONS
setopt autocd                  # change to dirs without cd
setopt autopushd               # automatically append dirs to the push/pop list
setopt pushd_ignore_dups       # and do not duplicate them
setopt pushd_to_home           # pushd with no args is like cd with no args.
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
setopt hist_ignore_dups        # ignore same commands run twice+
setopt appendhistory           # do not overwrite history 
setopt sharehistory            # share history between all running instances.
setopt hist_find_no_dups       # ignore dups in history search.
setopt extended_history        # store time info in history.
setopt interactive_comments    # escape commands so i can use them later
setopt print_exit_value        # alert me if something has failed
setopt nomatch                 # #fooo!
setopt noclobber               # do not overwrite files with >
setopt noflow_control          # disable control-q/control-s
setopt hashcmds                # avoid having to type 'rehash' all the time.
setopt rm_star_wait            # wait beforing ask if I want to delete all those files...
setopt multios                 # avoid having to use 'tee'
setopt checkjobs               # warn me about bg processes when exiting
setopt nohup                   # and do not kill them, either
setopt auto_continue           # automatically continue disowned jobs.
setopt auto_resume             # automatically resume jobs from commands
setopt transient_rprompt       # only show rprompt for current line.
## }}}
## {{{ KEY BINDINGS
bindkey -v

case $TERM in
    linux|screen*)
        bindkey "^[[1~" beginning-of-line
        bindkey "^[[3~" delete-char
        bindkey "^[[4~" end-of-line
        bindkey "^[[5~" up-line-or-history   # PageUp
        bindkey "^[[6~" down-line-or-history # PageDown
        bindkey "^[[A"  up-line-or-search    # up arrow for back-history-search
        bindkey "^[[B"  down-line-or-search  # down arrow for fwd-history-search
        bindkey "^?"   backward-delete-char
        bindkey "^H"   backward-delete-char
        bindkey "^[OM" accept-line
    ;;
    rxvt-unicode)
        bindkey "^[[7~" beginning-of-line  # home
        bindkey "^[[5~" up-line-or-history # pgup
        bindkey "^[[6~" down-line-or-history # pgdown
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
        bindkey "^[OM" accept-line
    ;;
esac

bindkey -a q quote-line
bindkey -a Q quote-region
bindkey -a 'H' run-help
bindkey "^_" copy-prev-shell-word
bindkey '^P' push-input
bindkey '^[h' run-help
bindkey '^r' vi-history-search-backward
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

## }}}
## {{{ FUNCTIONS
# Autoload various functions
autoload run-help compinit promptinit colors
autoload title shuffle perlpath prefix

function mdc { mkdr -p $1 && cd $1 }
function namedir { declare -g $1=$2; : ~$1 }
## }}}
## {{{ ALIASES
alias have='whence -p ls &>/dev/null'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias md="mkdir -p"
alias rd="rmdir"
alias df="df -h"
alias free="free -m"
alias la='ls -ax'
alias ll='ls -l'
alias lsd='ls -d *(/)'
alias l='ls -L'
alias vi=vim
alias gvi=gvim
alias vimrc="$EDITOR ~/.vimrc"
alias muttrc="$EDITOR ~/.mutt/muttrc"
alias zrc='vim ~/.zshrc'
alias zenv='vim ~/.zshenv' 
alias zpro='vim ~/.zprofile'
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
alias x2=nyxmms2
alias x2go='noglob nyxmms2 jump'
alias cnm='cnetworkmanager'
alias vw='vim ~docs/wiki/index.wiki'
alias gia='git add'
alias gis='git status'
alias gira='git rebase --amend'
alias girc='git rebase --continue'
alias gic='git commit'
alias dbs='dropbox status'
alias dbfs='dropbox filestatus'
alias cdd='cd ~/desk'
alias mplayer="title -e -- mplayer"
alias evince="title -e -- evince"
alias ssh="title -e -- ssh"
alias man="title -e -- man"

have todo.pl    && alias t=todo.pl
have pinfo      && alias info=pinfo
have ack-grep   && alias ack=ack-grep

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

mkdir -p ~/.cache/zsh

if [[ ! -f ~/.cache/zsh/alias-ls ]]; then
	# handle ls specially...
	local ls_cmd=ls
	local -a ls_args

	if have gls; then ls_cmd=gls; fi
	ls_args=('-Fh' '-H' '--color=auto' '--group-directories-first')

	while (( $#ls_args > 0 )); do
		if $ls_cmd $ls_args ~/.zsh &> /dev/null; then
			break
		else
			ls_args[-1]=()
		fi
	done
	echo "alias ls=\"$ls_cmd $ls_args\"" > ~/.cache/zsh/alias-ls 
	unset ls_cmd ls_args
fi

source ~/.cache/zsh/alias-ls
[[ -f ~/.local/perl/env.sh ]] && source ~/.local/perl/env.sh

case $OSTYPE in
	*gnu*)
		# use colors on gnu systems.
		alias grep='grep --color=auto'
		alias egrep='egrep --color=auto'
		alias fgrep='fgrep --color=auto'
	;;
	*bsd*)
		have gdircolors && alias dircolors=gdircolors
		if have gmake; then
    		alias make=gmake
    		alias bsdmake='command make'
		fi
	;;
esac

## }}}


# Add sbin directories for sudo tab completion.
zstyle ':completion:*:sudo:*' command-path $path /usr/sbin /sbin

# cache the output of completion functions.
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh

if have dircolors; then
	unset LS_COLORS
	eval $(dircolors ~/.dir_colors)
	# Colorize completions.
	zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# initialize advanced tab completion.
compinit -d ~/.cache/zsh/zcompdump

colors
promptinit   # Setup prompt theming 
prompt dylan # Set the prompt.

umask  077   # Create files that are readable only by moi
stty -ixon   # Disable the freeze-the-terminal-on-control-s thing.
ttyctl -f    # Freeze terminal properties.

namedir progfiles    ~/.wine/drive_c/Program\ Files
namedir dropbox      ~/.local/Dropbox
namedir docs         ~/.local/Dropbox/Documents
namedir trash        ~/.local/share/Trash

namedir moonshine    ~/code/moonshine

namedir g2           ~/work/g2
namedir hewitt       ~/work/hewitt
namedir arc          ~/work/hewitt/arc
namedir bes          ~/work/hewitt/bes
namedir bes2010      ~bes/BES-2010
namedir bes2011      ~bes/BES-2011
namedir bes2011_data ~bes/BES-2011-Data

if have xdg-user-dir; then
	[[ $PWD == $HOME ]] && pushd $(xdg-user-dir DESKTOP)
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
