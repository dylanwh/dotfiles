# put this file inside $HOME/.config/fish/functions

function printURIComponent
  awk 'BEGIN {while (y++ < 125) z[sprintf("%c", y)] = y
  while (y = substr(ARGV[1], ++j, 1))
  q = y ~ /[a-zA-Z0-9]/ ? q y : q sprintf("%%%02X", z[y])
  printf("%s", q)}' "$argv"
end

function textastic
    if test "$TMUX" = ""
        printf "\033]"
    else
        printf "\033Ptmux;\033\033]"
    end
    
	printf "6;textastic://?pwd="
	printURIComponent "$PWD"
	printf "&home="
	printURIComponent "$HOME"
	printf "&path="
	printURIComponent "$argv"
	
    if test "$TMUX" = ""
        printf "\a"
    else
        printf "\a\033\\"
    end
end
