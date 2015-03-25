# Dylan William Hardison's .zshrc file. #'
# This script is executed for every interactive shell.
# See also: ~/.zshenv ~/.zprofile [~/.zshrc] ~/.zlogin ~/.zlogout

# MUTAGEN {{{
source ~/.zsh/bundle/mutagen/mutagen.plugin.zsh
mutagen_infect ~/.zsh/bundle
fpath=(~/.zsh/lib $fpath)

# }}}
## {{{ VARIABLES
declare -g ZCACHE ZDATA
ZCACHE=$XDG_CACHE_HOME/zsh
ZDATA=$XDG_DATA_HOME/zsh
[[ -d $ZCACHE ]] || mkdir -p $ZCACHE
[[ -d $ZDATA ]]  || mkdir -p $ZDATA

HISTSIZE=4000
READNULLCMD=${PAGER:-/usr/bin/less}
LOGCHECK=30
SAVEHIST=3000
HISTFILE=$ZDATA/history

watch=(all)
fignore=(.o .hi .pyc)
cdpath=(~ ~/src/mozilla ~/Dropbox /media)

export PS_PERSONALITY=linux
export KEYTIMEOUT=1 # Kill lag for <esc> bindings.

if have dircolors; then
    unset LS_COLORS

    eval $(dircolors ~/.config/dircolors-solarized/dircolors.ansi-dark )
fi


## }}}
## {{{ OPTIONS
setopt autocd                  # change to dirs without cd
setopt autopushd               # automatically append dirs to the push/pop list
setopt pushd_ignore_dups       # and do not duplicate them
setopt pushd_to_home           # pushd with no args is like cd with no args.
setopt cdablevars              # the need for an explicit $
setopt autonamedirs            # any var that contains a full path is a named dir
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
setopt hist_ignore_all_dups
setopt appendhistory           # do not overwrite history
setopt sharehistory            # share history between all running instances.
setopt hist_find_no_dups       # ignore dups in history search.
setopt hist_save_no_dups
setopt hist_expire_dups_first
setopt extended_history        # store time info in history.
setopt interactive_comments    # escape commands so i can use them later
setopt no_print_exit_value     # prompt takes care of this
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
setopt no_list_beep
setopt nobeep

[[ $TERM = 'dumb' && $EMACS = t ]] && unsetopt zle

## }}}
## {{{ FUNCTIONS
# autoload useful functions distributed with zsh
autoload -U colors compinit edit-command-line insert-files promptinit run-help zcalc

# autoload my own utilities
autoload -U title shuffle perlpath prefix runbg fname mcp zle-sudo zle-less

# create zle bindings
if [[ -o zle ]]; then
    zle -N insert-files
    zle -N zle-sudo
    zle -N zle-less
    zle -N edit-command-line
fi

compinit -d $ZCACHE/zcompdump # initialize advanced tab completion
colors                        # initialize color associative arrays
promptinit                    # Setup prompt theming
prompt solarized              # Set the prompt.

function mdc      { mkdir -p $1 && cd $1 }
function namedir  { declare -g $1=$2; : ~$1  }
function save_cwd { echo $PWD >! $XDG_RUNTIME_DIR/last_cwd }
function hr       { seq -s ' ' 1 $COLUMNS | sed 's/[0-9]\+ \?/-/g' }
function bzfixperms { sudo chmod -Rc go+rX .; sudo chown -Rc dylan:http . }

precmd_functions+=( save_cwd )
## }}}
## {{{ KEY BINDINGS
if [[ -o zle ]]; then
    bindkey -v

    case $TERM in
        (linux|screen*)
            bindkey '^[[1~' beginning-of-line
            bindkey '^[[3~' delete-char
            bindkey '^[[4~' end-of-line
            bindkey '^[[5~' up-line-or-history   # PageUp
            bindkey '^[[6~' down-line-or-history # PageDown
            bindkey '^[[A'  up-line-or-search    # up arrow for back-history-search
            bindkey '^[[B'  down-line-or-search  # down arrow for fwd-history-search
            bindkey '^?'   backward-delete-char
            bindkey '^H'   backward-delete-char
            bindkey '^[OM' accept-line
            ;;
        (rxvt-unicode)
            bindkey '^[[7~' beginning-of-line  # home
            bindkey '^[[5~' up-line-or-history # pgup
            bindkey '^[[6~' down-line-or-history # pgdown
            bindkey '^[[8~' end-of-line        # end
            bindkey '^[[A' up-line-or-search   # up arrow
            bindkey '^[[B' down-line-or-search # down arrow
            bindkey '^?'   backward-delete-char
            bindkey '^H'   backward-delete-char
        ;;
    esac

    bindkey -M vicmd ' ' magic-space ## do history expansion on space
    bindkey -M vicmd '#' vi-pound-insert
    bindkey -M vicmd Q   quote-line
    bindkey -M vicmd q   quote-region
    bindkey -M vicmd u   undo
    bindkey -M vicmd v   edit-command-line
    bindkey -M vicmd k   up-line-or-search
    bindkey -M vicmd j   down-line-or-search

    bindkey '^_' copy-prev-shell-word
    bindkey '^Q' push-input
    bindkey '^E' expand-word
    bindkey '^ ' _expand_alias

    if have fasd; then
        bindkey -r '^X'
        bindkey '^X^A' fasd-complete
        bindkey '^X^F' fasd-complete-f
        bindkey '^X^D' fasd-complete-d
    fi

    bindkey '^O'   accept-and-infer-next-history
    bindkey '^[^M' accept-and-hold
    bindkey '^F'   insert-files

    bindkey '^P' up-history
    bindkey '^N' down-history
    bindkey '^?' backward-delete-char
    bindkey '^H' backward-delete-char
    bindkey '^W' backward-kill-word
    bindkey '^R' history-incremental-search-backward
    bindkey '^S' zle-sudo
fi
## }}}
## {{{ ALIASES
alias ack='noglob ack'
alias cp='cp -i'
alias cpanm-test='command cpanm'
alias cpanm='cpanm --notest'
alias dbfs='dropbox filestatus'
alias dbs='dropbox status'
alias df="df -h"
alias egrep='egrep --color=auto'
alias evince='runbg evince'
alias fd='fasd_cd -d'
alias ff='find . -type f -name'
alias fgrep='fgrep --color=auto'
alias find="noglob find"
alias free="free -h"
alias g='git'
alias gcd='cd $(git top)'
alias grep='grep --color=auto'
alias help=run-help
alias l='ls -L'
alias la='ls -ax'
alias ll='ls -l'
alias lsd='ls -d *(/)'
alias md="mkdir -p"
alias muttrc="$EDITOR ~/.mutt/muttrc"
alias mv='mv -i'
alias nl0="tr '\n' '\0'"
alias pd=popd
alias pdoc=perldoc
alias pu=pushd
alias rd="rmdir"
alias rm='rm -i'
alias vi=emacsclient
alias xrc="$EDITOR ~/.xinitrc"
alias xs=cd
alias zenv="$EDITOR ~/.zshenv"
alias zpro="$EDITOR ~/.zprofile"
alias zrc="$EDITOR ~/.zshrc"
alias zreload='exec env SHLVL=0 $SHELL'
alias home-time='TZ=US/Eastern date'
alias emacsclient='emacsclient -c'

have pinfo    && alias info=pinfo
have ack-grep && alias ack=ack-grep
have gcp      && alias cp=gcp
have hub      && eval "$(hub alias -s)"
have fasd     && eval "$(fasd --init auto)"
have mosh     && alias mosh=$'mosh --ssh=\'ssh -o ClearAllForwardings=yes\''

have systemctl && alias systemctl='sudo systemctl'
have pacman    && alias pacman='sudo pacman'
have yaourt    && alias yaourt='yaourt --noconfirm'

alias xclip='xclip -selection clipboard'

# paste
alias p='xclip -o'
alias P='xclip -o |'

# copy
alias c='xclip -i'
alias -g C='| xclip -i'
alias -g @='$( xclip -o )'
alias -g '"@"'='"$( xclip -o )"'

# copy and paste
have vipe && alias pvc='p | vipe | c'

# less
alias -g L='| less -F'

# grep
alias -g 'G'='|grep '

# scripts
alias -s pl='perl -S'
alias -s py=python
alias -s rb=ruby
alias -s hs=runhaskell

# alias ls to ls -Fh --color=auto --group-directories-first,
# but if our version of ls does not support one or more of those,
# then don't use it.
if [[  ~/.zshrc -nt $ZCACHE/alias-ls || ! -f $ZCACHE/alias-ls ]]; then
    # handle ls specially...
    local ls_cmd=ls
    local -a ls_args

    if have gls; then ls_cmd=gls; fi
    ls_args=('-Fh' '--color=auto' '--group-directories-first')

    while (( $#ls_args > 0 )); do
        if $ls_cmd $ls_args ~/.zsh &> /dev/null; then
            break
        else
            ls_args[-1]=()
        fi
    done
    echo "alias ls=\"$ls_cmd $ls_args\"" >! $ZCACHE/alias-ls
    unset ls_cmd ls_args
fi

source $ZCACHE/alias-ls
## }}}
# {{{ ZSTYLES
if [[ -o zle ]]; then
    # Add sbin directories for sudo tab completion.
    zstyle ':completion:*:sudo:*' command-path $path /usr/sbin /sbin

    # cache the output of completion functions.
    zstyle ':completion:*' use-cache on
    zstyle ':completion:*' cache-path $ZCACHE

    # use menu style completions
    zstyle ':completion:*' menu 'select>10'
fi

# colorize file listing from completions
[[ -n $LS_COLORS ]] && zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# }}}
# NAMED DIRECTORIES# {{{
namedir progfiles     ~/.wine/drive_c/Program\ Files
namedir moonshine     ~/src/moonshine

if have xdg-user-dir; then
    namedir docs   $(xdg-user-dir DOCUMENTS)
    namedir desk   $(xdg-user-dir DESKTOP)
    namedir pub    $(xdg-user-dir PUBLICSHARE)
    namedir mus    $(xdg-user-dir MUSIC)
    namedir pics   $(xdg-user-dir PICTURES)
    namedir vids   $(xdg-user-dir VIDEOS)
    namedir dl     $(xdg-user-dir DOWNLOAD)
fi
# }}}

umask  077   # Create files that are readable only by moi
stty -ixon   # Disable the freeze-the-terminal-on-control-s thing.
ttyctl -f    # Freeze terminal properties.

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh/lib,~/:
