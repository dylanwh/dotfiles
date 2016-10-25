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

LS_COLORS=''
unset LS_COLORS
for dircolors in dircolors gdircolors; do
    if have $dircolors; then
        eval $($dircolors ~/.config/dircolors/zenburn)
        break
    fi
done

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

function mdc      { mkdir -p $1 && cd $1 }
function namedir  { declare -g $1=$2; : ~$1  }
function save_cwd { echo $PWD >! $XDG_RUNTIME_DIR/last_cwd }
function hr       { seq -s ' ' 1 $COLUMNS | sed 's/[0-9]\+ \?/-/g' }
function bzfixperms { sudo chmod -Rc go+rX .; sudo chown -Rc dylan:http . }

if have bz; then
    function cdb {
        cd $(bz path "$@")
        bz summary
    }
else
    function bz { ssh -t bugzilla.vm bz "$@" }
fi

precmd_functions+=( save_cwd )
## }}}
## {{{ KEY BINDINGS
if [[ -o zle ]]; then
    bindkey -e

    bindkey ' ' magic-space ## do history expansion on space

    bindkey '^O'   accept-and-infer-next-history
    bindkey '^[^M' accept-and-hold
    bindkey '^F'   insert-files
fi
## }}}
## {{{ ALIASES
alias cp='cp -i'
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
alias xrc="$EDITOR ~/.xinitrc"
alias xs=cd
alias zenv="$EDITOR ~/.zshenv"
alias zpro="$EDITOR ~/.zprofile"
alias zrc="$EDITOR ~/.zshrc"
alias zreload='exec env SHLVL=0 $SHELL'
alias home-time='TZ=US/Eastern date'
alias emacsclient='emacsclient -a ""'
alias ec='emacsclient -c'
alias et='emacsclient -t'
alias sudo='command sudo '
alias P='p |'
alias -g C='| c'
alias -g L='| less -F'
alias -g 'G'='|grep '

PLATFORM_ALIAS_FILE=$ZCACHE/platform_alias

function platform_alias {
    local def=${1//\'/\\\'}
    echo "alias $'$def'" >> $PLATFORM_ALIAS_FILE
}

if [[  ~/.zshrc -nt $PLATFORM_ALIAS_FILE || ! -f $PLATFORM_ALIAS_FILE ]]; then
    echo -n >! $PLATFORM_ALIAS_FILE

    have pinfo    && platform_alias info=pinfo
    have ack-grep && platform_alias ack=ack-grep
    have gcp      && platform_alias cp='gcp -i'
    have gmv      && platform_alias mv='gmv -i'
    have grm      && platform_alias rm='grm -i'
    have gfind    && platform_alias find='noglob gfind'
    have hub      && platform_alias git=hub
    have vipe     && platform_alias pvc='p | vipe | c'

    if have docker; then
        platform_alias runti='docker run --rm -ti'
    fi
    if have docker-machine; then
        platform_alias dmdf='docker-machine ssh default df -h /mnt/sda1'
    fi
    if have cpanm; then
        platform_alias cpanm-test='command cpanm'
        platform_alias cpanm='cpanm --notest'
    fi

    have systemctl && platform_alias systemctl='sudo systemctl'
    have pacman    && platform_alias pacman='sudo pacman'
    have yaourt    && platform_alias yaourt='yaourt --noconfirm'

    if have xclip; then
        platform_alias xclip='xclip -selection clipboard'
        platform_alias c='xclip -i'
        platform_alias p='xclip -o'
    elif have pbcopy; then
        platform_alias c='pbcopy'
        platform_alias p='pbpaste'
    fi

    have dnf && platform_alias yum=dnf

    # alias ls to ls -Fh --color=auto --group-directories-first,
    # but if our version of ls does not support one or more of those,
    # then don't use it.
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
    platform_alias ls="$ls_cmd $ls_args"
    unset ls_cmd ls_args
fi

source $PLATFORM_ALIAS_FILE

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
namedir bmo ~/src/mozilla/bmo

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

if [[ $TERM == xterm ]]; then
    ppid="$(cat /proc/$$/stat | cut -d ' ' -f 4)"
    xterm="$(readlink /proc/$ppid/exe)"

    case $xterm in
        (*gnome*terminal*) TERM=xterm-256color ;;
    esac
    export TERM
    unset xterm
    unset ppid
fi

if [[ $TERM = 'dumb' ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
else
    prompt boring # Set the prompt.
    [[ -f "$HOME/.iterm2_shell_integration.zsh" ]] && source "$HOME/.iterm2_shell_integration.zsh"
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh/lib,~/:
