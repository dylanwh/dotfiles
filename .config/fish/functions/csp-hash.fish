# Defined in - @ line 0
function csp-hash --description 'alias csp-hash=echo sha256-(pbpaste | openssl dgst -sha256 -binary | base64)'
	echo sha256-(pbpaste | openssl dgst -sha256 -binary | base64) $argv;
end
