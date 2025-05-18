set -x REALNAME "Dylan Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x LC_TIME POSIX
set -x TZ America/Los_Angeles
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
            and test -n "$selenized_variant"
            and echo -e "\033]1337;SetColors=preset=selenized-$selenized_variant\a"
            if have emacs
                set -x EDITOR emacsclient -a "" -t -q
                abbr -a -g -- vi emacs
                abbr -a -g -- vim emacs
            else
                set -x EDITOR vim
                abbr -a -g vi vim
            end
    end

    source $HOME/.config/fish/abbr.fish
end

if have fasd
    function __fasd_run -e fish_preexec
        command fasd --proc (command fasd --sanitize "$argv" | tr -s ' ' \n) >/dev/null 2>&1 &
        disown
    end
end

test -f $HOME/.config/fish/local.fish
and source $HOME/.config/fish/local.fish
