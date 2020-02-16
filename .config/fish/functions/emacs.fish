# Defined in - @ line 1
function emacs --description 'alias emacs=emacsclient -a "" -t'
	emacsclient -a "" -t $argv;
end
