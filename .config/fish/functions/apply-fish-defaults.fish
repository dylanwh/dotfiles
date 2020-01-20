function apply-fish-defaults --description 'apply one-time fish configuration stuff'
    echo "applying fish defaults"

    set -U fish_greeting ''

    path clear

    path add /snap/bin
    path add ~/bin
    path add ~/.plenv/bin
    path add ~/.plenv/shims
    path add ~/.pyenv/shims
    path add ~/.cargo/bin
    path add /opt/chefdk/bin
    path add ~/.chefdk/gem/ruby/2.5.0/bin
    path add /opt/chefdk/embedded/bin
    path add (go env GOPATH)/bin
    path prune

    source (dircolors -c | sed 's/setenv/set -Ux/' | psub)
    colorload
    abbrload
end
