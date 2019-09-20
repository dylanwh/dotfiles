function apply-fish-defaults --description 'apply one-time fish configuration stuff'
    # bump this if any substantial changes are made
    set -l config_version 4

    test -n "$dylan_config_version"
    or return 0

    test "$config_version" -gt "$dylan_config_version"
    or return 0

    set -U dylan_config_version $config_version
    echo "applying fish defaults"

    set -Ux GIT_CEILING_DIRECTORIES "$HOME/Code"

    set -Ux REALNAME "Dylan William Hardison"
    set -Ux EMAIL "dylan@hardison.net"
    set -Ux MANPAGER 'less -s'
    set -eg LANG
    set -Ux LANG en_US.UTF-8
    set -Ux LC_COLLATE POSIX # sort in POSIX order.
    set -Ux TZ US/Eastern

    set -U fish_greeting

    set -l emacsclient (which emacsclient)
    set -eg EDITOR
    set -Ux ALTERNATE_EDITOR ''
    set -Ux EDITOR "$emacsclient -t"

    if [ -d ~/bin ]
        path add ~/bin
    end

    for gnubin in /usr/local/opt/*/libexec/gnubin
        path add $gnubin
    end

    if [ -d ~/.plenv/bin ]
        path add ~/.plenv/bin ~/.plenv/shims

    end

    if [ -d ~/.pyenv/shims ]
        path add ~/.pyenv/shims
    end

    if [ -d ~/.cargo/bin ]
        path add ~/.cargo/bin
    end

    set -Ux GOPATH ~/go
    path add $GOPATH/bin

    source (dircolors -c | sed 's/setenv/set -Ux/' | psub)
    colorload
    abbrload
end
