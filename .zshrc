# Dylan William Hardison's .zshrc file.
# This script is executed for every interactive shell.
# See also: ~/.zshenv ~/.zprofile [~/.zshrc] ~/.zlogin ~/.zlogout

## {{{ Variables
HISTSIZE=2000
SAVEHIST=2000
HISTFILE=~/.zhistory
READNULLCMD=${PAGER:-/usr/bin/pager}
LOGCHECK=30
watch=(all)
fignore=(.o .hi .pyc)
cdpath=(~ ~/src)
fpath=(~/.zsh $fpath)

## }}}
## {{{ Options
setopt autocd                  # change to dirs without cd
setopt autopushd               # automatically append dirs to the push/pop list
setopt pushdignoredups         # and don't duplicate them
setopt nocdablevars            # the need for an explicit $
setopt checkjobs               # warn me about bg processes when exiting
setopt nohup                   # and don't kill them, either
setopt listpacked              # compact completion lists
setopt nolisttypes             # show types in completion
setopt extended_glob           # weird & wacky pattern matching - yay zsh!
setopt alwaystoend             # when complete from middle, move cursor
setopt completeinword          # not just at the end
setopt glob_complete           # complete globs with a menu.
setopt nocorrect               # spelling correction
setopt promptcr                # add \n which overwrites cmds with no \n
setopt histverify              # when using ! cmds, confirm first
setopt interactivecomments     # escape commands so i can use them later
setopt printexitvalue          # alert me if something's failed
setopt hist_ignore_dups        # ignore same commands run twice+
setopt appendhistory           # don't overwrite history 
setopt nomatch                 # #fooo!
setopt noclobber               # don't overwrite files with >
setopt sharehistory            # share history between all running instances.
setopt hist_find_no_dups       # ignore dups in history search.
setopt noflow_control          # disable control-q/control-s
setopt hashcmds                # avoid having to type 'rehash' all the time.
setopt rm_star_wait            # wait beforing ask if I want to delete all those files...
setopt multios                 # avoid having to use 'tee'
## }}}
## {{{ Key Bindings
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
bindkey -v "^_" copy-prev-shell-word
bindkey '^P' push-input
bindkey -v

## }}}
## {{{ Functions 
function have { which $1 &>/dev/null }
## }}}
## {{{ Aliases
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias gvi=gvim
alias ls='ls --color=auto -F -h'
alias grep='egrep --color=auto'
alias ggrep='command grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias l='ls -x -I_darcs'
alias la='ls -ax'
alias ll='ls -l'
alias vi=vim
alias vimrc="$EDITOR ~/.vimrc"
alias muttrc="$EDITOR ~/.mutt/muttrc"
alias zrc='vim ~/.zshrc'
alias zenv='vim ~/.zshenv' 
alias xrc='vim ~/.xinitrc'
alias xmrc='vim ~/.xmonad/xmonad.hs'
alias xs=cd
alias zreload='exec $SHELL'
alias help=run-help
alias pdoc=perldoc
alias g="sr google"
alias pd=popd
alias todo=yodo
alias tda="todo -a"
alias tdr="todo -r"
alias tdd="todo -d"
alias tde="todo -e"
alias find="noglob find"
unalias run-help
## }}}

# Autoload various functions
autoload sshbegin sshend run-help
autoload compinit promptinit

# initialize advanced tab completion.
compinit -d ~/.zcompdump

# Add sbin directories for sudo's tab completion.
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
	/usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

promptinit           # Setup prompt theming 
prompt dylan         # Set the prompt.

umask  022           # Create files that are read-only by group.
stty -ixon           # Disable the freeze-the-terminal-on-control-s thing.
mesg   yes           # Allow messages
ttyctl -f            # Freeze terminal properties.

have pinfo && alias info=pinfo
if have dircolors; then
	unset LS_COLORS
	eval $(dircolors ~/.dir_colors)
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
