# Defined in - @ line 1
if have hub
    function git --wraps=hub --description 'alias git hub'
        hub  $argv;
    end
else
    function git --wraps=git --description 'alias git hub'
        command git $argv
    end
end
