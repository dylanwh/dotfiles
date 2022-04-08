set -x REALNAME "Dylan Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x TZ US/Pacific
set -x GOPATH $HOME/.local/go
set -g OS (uname)
set --erase CDPATH

if status --is-interactive
    set -g shell_parent (ps -o ppid= $fish_pid | xargs ps -o comm=)
    set --erase shell_via

    switch "$shell_parent"
        case 'mosh*'
            set -x shell_via mosh
        case 'sshd*' 'login' '/usr/bin/login' 'tmux*'
            smart-ssh-agent
    end

    switch "$TERM_PROGRAM"
        case vscode
            set code code
            if string match -q "*insider" $TERM_PROGRAM_VERSION
                abbr -a -g -- code code-insiders
                set code code-insiders
            end
            set -x EDITOR "$code -w"
            abbr -a -g -- vi $code
            abbr -a -g -- vim $code
        case '*'
            set -x EDITOR "vim"
            abbr -a -g vi vim
    end
end

test -f $HOME/.config/fish/local.fish
and source $HOME/.config/fish/local.fish
