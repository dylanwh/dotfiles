# put this file inside $HOME/.config/fish/functions

function openUrl
  if not set -q argv[1]
    echo 'Usage: openUrl <url>'
    echo
    echo Open URL on iOS.
    return 0
  end
    
  set FIFO $(mktemp)
  rm -f $FIFO
  mkfifo $FIFO
 
  if test "$TMUX" = ""
    printf "\033]"
  else
    printf "\033Ptmux;\033\033]"
  end
 
  printf "6;open://?ver=2&respond="
  echo -n "$FIFO" | base64
  printf "&url="
  echo -n "$argv[1]" | base64

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
