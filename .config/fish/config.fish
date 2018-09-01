set -x GIT_CEILING_DIRECTORIES "$HOME/src"

set -x REALNAME "Dylan William Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x TZ US/Eastern

set fish_greeting

alias have="command -sq"
alias zreload='exec fish'

have cpanm;     and alias cpanm='cpanm --notest'
have hub;       and alias git=hub
have docker;    and alias runti='docker run --rm -ti'
have git-annex; and alias gan='git annex'
have gmake;     and alias make='gmake'
have gdate;     and alias date='gdate'
have gfind;     and alias find='gfind'
have gcp;       and alias cp='gcp -i'
have gmv;       and alias mv='gmv -i'
have grm;       and alias rm='grm -i'

set ls_cmd ls
have gls; and set ls_cmd gls
eval "alias ls='$ls_cmd -Fh --color=auto --group-directories-first'"

set dircolors_cmd dircolors
have gdircolors; and set dircolors_cmd gdircolors
eval "eval ($dircolors_cmd -c)"

set --erase -g EDITOR
if have emacsclient
  set -l emacsclient (which emacsclient)
  set -Ux EDITOR "$emacsclient -t -a \"\""
else
  set -Ux EDITOR "nano"
end

if have plenv
    source (plenv init -|psub)
end
if have pyenv
    source (pyenv init -|psub)
end
if have pyenv-virtualenv
    source (pyenv virtualenv-init -|psub)
end
