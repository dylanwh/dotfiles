set -x REALNAME "Dylan William Hardison"
set -x EMAIL "dylan@hardison.net"
set -x MANPAGER 'less -s'
set -x LANG en_US.UTF-8
set -x LC_COLLATE POSIX # sort in POSIX order.
set -x TZ US/Eastern
set -x GOPATH $HOME/go
set -g OS (uname)

if [ -x /usr/libexec/path_helper ]
    source (env -i /usr/libexec/path_helper -c | psub)
end
set -U fish_user_paths $fish_user_paths

set -g shell_parent (ps -o ppid= $fish_pid | xargs ps -o comm=)

if have caffeinate; and have nq
    switch $shell_parent
        case 'ssh*' 'mosh*'
            mkdir -p $HOME/.cache
            # I'm not sure why, but if I don't run nq inside nq, it sometimes fails to clean up its files
            # Might be a bug, or something about how macOS handles the session leader being teriminated?
            # Anyway, this pretty much ensures remove logins prevent the mac from going to sleep (as long as it is running on AC)
            env "NQDIR=$HOME/.cache/caffinate" nq -c -q -- nq -c -q -- caffeinate -s -w $fish_pid
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

set -l config_version (perl -MFile::stat -E 'say stat($ARGV[0])->mtime' ~/.config/fish/functions/apply-fish-defaults.fish)
test -z "$dylan_config_version"
or test "$config_version" -gt "$dylan_config_version"
and apply-fish-defaults
set -U dylan_config_version $config_version

if path empty
    path default
end

