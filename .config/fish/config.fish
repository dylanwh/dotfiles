alias emacsclient='emacsclient -a ""'
alias ec='emacsclient -c'
alias et='emacsclient -t'

alias have='command -sq'

have hub; and alias git=hub
have docker; and alias runti='docker run --rm -ti'
have git-annex; and alias gan='git annex'
have gfind; and alias find='gfind'
have gcp; and alias cp='gcp -i'
have gmv; and alias mv='gmv -i'
have grm; and alias rm='grm -i'
have cpanm; and alias cpanm='cpanm --notest'

set ls_cmd ls
have gls; and set ls_cmd gls

eval "alias ls='$ls_cmd -Fh --color=auto --group-directories-first'"

set dircolors_cmd dircolors
have gdircolors; and set dircolors_cmd gdircolors
eval "eval ($dircolors_cmd -c)"

source ~/.config/fish/colors.fish

fundle plugin edc/bass

have bass;
and test -d /opt/rh/sclo-git25
and bass source /opt/rh/sclo-git25/enable

# test $TERM_PROGRAM = iTerm.app
# and test -e {$HOME}/.iterm2_shell_integration.fish
# and source {$HOME}/.iterm2_shell_integration.fish
