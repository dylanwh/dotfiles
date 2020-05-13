function apply-fish-defaults --description 'apply one-time fish configuration stuff'
    echo "applying fish defaults"

    set -U fish_greeting ''

    path clear
    path default

    set --erase -g LS_COLORS
    set --erase -U LS_COLORS
    for dircolors in dircolors gdircolors
        if have $dircolors
            $dircolors -c | sed 's/setenv/set -Ux/' | source
            set -Ux LS_COLORS "$LS_COLORS:ow=1;7;34:st=30;44:su=30;41"
        end
    end
    test -z $LS_COLORS
    and echo "no dircolors"
    colorload
    abbrload
end
