if status is-interactive
    function git --wraps=hub --description 'alias git hub'
        hub  $argv
    end
else
    function git
        command git $argv
    end
end
