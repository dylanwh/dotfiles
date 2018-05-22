function uplift
	for file in $argv
scp $file web:$file
end
end
