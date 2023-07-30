# put this file inside $HOME/.config/fish/functions

function textastic
  if not set -q argv[1]
    echo 'Usage: textastic <text-file>'
    echo
    echo Open in Textastic 9.5 or later.
    echo File must be in directory represented in the Files app to allow writing back edits.
    return 0
  end
  
  # make sure file exists
  if not test -e "$argv[1]"
    touch "$argv[1]"
    sleep 1
  end
  
  if test "$TMUX" = ""
    printf "\033]"
  else
    printf "\033Ptmux;\033\033]"
  end
    
  printf "6;textastic://?ver=2&pwd="
  echo -n "$PWD" | base64
  printf "&home="
  echo -n "$HOME" | base64
  printf "&path="
  echo -n "$argv" | base64
	
  if test "$TMUX" = ""
    printf "\a"
  else
    printf "\a\033\\"
  end
end
