function abbrupdate
	source (abbr | awk '!/-a -g --/ { print "abbr --erase", $5 }'|psub); abbrload
end
