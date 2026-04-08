on open location theURI
	-- This handles org-protocol:// links from Safari or Bookmarklets
	do shell script "~/.local/bin/with-nix-env emacsclient " & quoted form of theURI
end open location

on open theFiles
	set pathList to {}
	repeat with aFile in theFiles
		set end of pathList to quoted form of POSIX path of (aFile as text)
	end repeat
	
	set AppleScript's text item delimiters to space
	set concatFiles to pathList as text
	set AppleScript's text item delimiters to ""
	
	do shell script "~/.local/bin/with-nix-env emacsedit " & concatFiles
	
	return
end open

on run
	open (choose file with multiple selections allowed)
end run
