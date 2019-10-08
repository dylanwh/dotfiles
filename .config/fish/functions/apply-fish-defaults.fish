function apply-fish-defaults --description 'apply one-time fish configuration stuff'
    echo "applying fish defaults"

    set -Ux GIT_CEILING_DIRECTORIES "$HOME/Code"

    set -Ux REALNAME "Dylan William Hardison"
    set -Ux EMAIL "dylan@hardison.net"
    set -Ux MANPAGER 'less -s'
    set -eg LANG
    set -Ux LANG en_US.UTF-8
    set -Ux LC_COLLATE POSIX # sort in POSIX order.
    set -Ux TZ US/Eastern

    set -U fish_greeting ''

    set -l emacsclient (which emacsclient)
    set -eg EDITOR
    set -Ux ALTERNATE_EDITOR ''
    set -Ux EDITOR "$emacsclient -t"

    set -Ux GOPATH ~/go

    path clear

    path add ~/bin
    for gnubin in /usr/local/opt/*/libexec/gnubin
        path add  $gnubin
    end

    path add /snap/bin

    path add ~/.plenv/bin
    path add ~/.plenv/shims
    path add ~/.pyenv/shims
    path add ~/.cargo/bin
    path add /opt/chefdk/bin
    path add ~/.chefdk/gem/ruby/2.5.0/bin
    path add /opt/chefdk/embedded/bin
    path add $GOPATH/bin
    path prune

    source (dircolors -c | sed 's/setenv/set -Ux/' | psub)
    colorload
    abbrload
end
