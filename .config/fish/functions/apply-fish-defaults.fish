function apply-fish-defaults --description 'apply one-time fish configuration stuff'
    echo "applying fish defaults"

    set -U fish_greeting ''

    path clear
    path default

    have dircolors
    and source (dircolors -c | sed 's/setenv/set -Ux/' | psub)
    colorload
    abbrload
end
