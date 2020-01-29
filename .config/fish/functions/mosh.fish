# Defined in - @ line 1
function mosh --description 'alias mosh env MOSH_TITLE_NOPREFIX=1 mosh -6'
	env MOSH_TITLE_NOPREFIX=1 mosh -6 $argv;
end
