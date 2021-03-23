set -x REALNAME "Dylan Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x TZ US/Eastern
set -x GOPATH $HOME/.local/go
set -g OS (uname)

test -x /usr/libexec/path_helper
and source (env -i /usr/libexec/path_helper -c | psub)

set -U fish_user_paths $fish_user_paths

set -g shell_parent (ps -o ppid= $fish_pid | xargs ps -o comm=)
set --erase shell_via

switch $shell_parent
    case 'mosh*'
        set -x shell_via mosh
    case 'sshd*'
        ssh-init adopt
    case 'login' '/usr/bin/login'
        switch $OS
            case Darwin
                ssh-add -q -A
                or for file in ~/.ssh/id_ed25519 ~/.ssh/id_rsa
                    ssh-add -K $file
                end
        end
end

switch $shell_parent
    case 'ssh*' 'mosh*'
        if have caffeinate; and have nq
            mkdir -p $HOME/.cache
            # I'm not sure why, but if I don't run nq inside nq, it sometimes fails to clean up its files
            # Might be a bug, or something about how macOS handles the session leader being teriminated?
            # Anyway, this pretty much ensures remove logins prevent the mac from going to sleep
            # (as long as it is running on AC)
            env "NQDIR=$HOME/.cache/caffinate.$fish_pid" nq -c -q -- nq -c -q -- caffeinate -s -w $fish_pid
        end
end

if status --is-interactive
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
