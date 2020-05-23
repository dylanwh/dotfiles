# Defined in - @ line 1

function git --description 'alias git=hub'
    if [ "$PWD" = "$HOME" ]
        set argv "-C" ~/Git/dylanwh/home $argv
    end
    hub $argv;
end
