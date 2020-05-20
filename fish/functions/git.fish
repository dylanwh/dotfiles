# Defined in - @ line 1

function git --description 'alias git=hub'
    if [ -d .homegit ]
        set argv --git-dir=$HOME/.homegit $argv
    end
    hub $argv;
end
