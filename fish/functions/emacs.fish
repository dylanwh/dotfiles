# Defined in - @ line 1
function emacs --description 'alias emacs=emacsclient -a "" -t'
    emacsedit --no-wait $argv
end
