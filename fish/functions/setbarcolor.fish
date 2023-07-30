# put this file inside $HOME/.config/fish/functions

function ios_sequence
  set --local output "$(
    if set -q TMUX
      printf '\033Ptmux;\033\033]'
      echo -n $argv[1] | tr -d '[:space:]'
      printf '\a\033\\'
    else
      printf '\033]'
      echo -n $argv[1] | tr -d '[:space:]'
      printf '\a'
    end)"
  echo $output
end

function setbarcolor
    if not set -q argv[1]
        echo 'Usage: setbarcolor <css-style color>'
        echo ''
        echo 'Set color of terminal toolbar color with values such as'
        echo '  red, #f00, #ff0000, rgb(255,0,0), color(p3 1.0 0.0 0.0)'
    else
        ios_sequence "$(
          awk 'BEGIN {printf "6;settoolbar://?ver=2&color="}'
          echo -n $argv[1] | base64)"
    end
end

