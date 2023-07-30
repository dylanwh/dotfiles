# put this file inside $HOME/.config/fish/functions

function pbpaste
  # paste from iOS device clipboard to standard output
  set FIFO $(mktemp)
  rm -f $FIFO
  mkfifo $FIFO
 
  if test "$TMUX" = ""
    printf "\033]"
  else
    printf "\033Ptmux;\033\033]"
  end
    
  printf "6;pbpaste://?ver=2&respond="
  echo -n "$FIFO" | base64

  if test "$TMUX" = ""
    printf "\a"
  else
    printf "\a\033\\"
  end
 
  read <$FIFO -s REPLY
  rm -f $FIFO
  
  if test -z "$REPLY"
    return 0
  end

  if string match -q "error=*" "$REPLY"
    echo (string replace -r "^error=" "" "$REPLY") | base64 -d
    return 1
  end
  
  if string match -q "result=*" "$REPLY"
    echo (string replace -r "^result=" "" "$REPLY") | base64 -d
  end
end
