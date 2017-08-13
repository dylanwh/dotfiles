set PATH $HOME/bin $PATH

alias have="command -sq"
alias zreload='exec fish'

have cpanm; and alias cpanm='cpanm --notest'
have hub; and alias git=hub
have docker; and alias runti='docker run --rm -ti'
have git-annex; and alias gan='git annex'
have gfind; and alias find='gfind'
have gcp; and alias cp='gcp -i'
have gmv; and alias mv='gmv -i'
have grm; and alias rm='grm -i'

set ls_cmd ls
have gls; and set ls_cmd gls
eval "alias ls='$ls_cmd -Fh --color=auto --group-directories-first'"

set dircolors_cmd dircolors
have gdircolors; and set dircolors_cmd gdircolors
eval "eval ($dircolors_cmd -c)"
fundle plugin edc/bass
fundle plugin oh-my-fish/plugin-tab
fundle plugin oh-my-fish/plugin-emacs

switch (uname)
  case Darwin
    fundle plugin oh-my-fish/plugin-osx
    have brew; and fundle plugin oh-my-fish/plugin-brew
end

fundle init

if functions -q bass
    test -d /opt/rh/sclo-git25; and bass source /opt/rh/sclo-git25/enable
end

if have grc
  set -U grc_plugin_execs cvs df diff dig gcc g++ ifconfig \
    make mount mtr netstat ping ps tail traceroute \
    wdiff blkid du dnf docker docker-machine id ip iostat \
    last lsattr lsblk lspci lsmod lsof getfacl getsebool ulimit uptime nmap \
    fdisk findmnt free semanage ss sysctl systemctl stat showmount tune2fs \
    tcpdump tune2fs \
    vmstat w who

  for executable in $grc_plugin_execs
    function $executable --inherit-variable executable --wraps=$executable
      grc $executable $argv
    end
  end
end

have plenv; and source (plenv init -|psub)

function itermize
  test -e {$HOME}/.iterm2_shell_integration.fish
  and source {$HOME}/.iterm2_shell_integration.fish
end

test "$TERM_PROGRAM" = iTerm.app; and itermize

abbr --add lofn env MOSH_TITLE_NOPREFIX=1 mosh lofn
abbr --add vi et
abbr --add vim et
