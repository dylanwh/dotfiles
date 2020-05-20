function stdiff --argument file
	set -l old_file (string replace -r '\.sync-conflict-\d+-\d+-\w+' '' $file)
diff -u $file $old_file
end
