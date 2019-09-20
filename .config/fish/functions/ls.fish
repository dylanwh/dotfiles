# Defined in - @ line 1
function ls --description 'alias ls=ls -Fh --color=auto --group-directories-first'
	command ls -Fh --color=auto --group-directories-first $argv;
end
