set -x REALNAME "Dylan Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x TZ US/Pacific
set -g OS (uname)
set --erase CDPATH

if status --is-interactive
    set -g shell_parent (ps -o ppid= $fish_pid | xargs ps -o comm=)
    set --erase shell_via

    switch "$shell_parent"
        case 'mosh*'
            set -x shell_via mosh
        case 'sshd*' login /usr/bin/login 'tmux*'
            if [ -z "$SSH_AUTH_SOCK" ]
                set -x SSH_AUTH_SOCK (ssh-auth-sock)
            end
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
            test "$LC_TERMINAL" = iTerm2
            and test -f ~/.config/fish/iterm2_colors
            and cat ~/.config/fish/iterm2_colors
            if have nvim
                set -x EDITOR nvim
                abbr -a -g -- vi nvim
                abbr -a -g -- vim nvim
            else
                set -x EDITOR vim
                abbr -a -g vi vim
            end
    end
end

if have fasd
  function __fasd_run -e fish_preexec
    command fasd --proc (command fasd --sanitize "$argv" | tr -s ' ' \n) > "/dev/null" 2>&1 &; disown
  end
end

test -f $HOME/.config/fish/local.fish
and source $HOME/.config/fish/local.fish
