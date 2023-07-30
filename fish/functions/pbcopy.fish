# put this file inside $HOME/.config/fish/functions

function pbcopy
  # copy standard input or arguments to iOS clipboard
  if test "$TMUX" = ""
    printf "\033]"
  else
    printf "\033Ptmux;\033\033]"
  end
    
  printf "52;c;"
  if [ (count $argv) -eq 0 ]
    base64 | tr -d '\n'
  else
    echo -n "$argv" | base64 | tr -d '\n'
  end
	
  if test "$TMUX" = ""
    printf "\a"
  else
    printf "\a\033\\"
  end
end
