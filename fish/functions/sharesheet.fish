function printURIComponent
  awk 'BEGIN {while (y++ < 125) z[sprintf("%c", y)] = y
  while (y = substr(ARGV[1], ++j, 1))
  q = y ~ /[a-zA-Z0-9]/ ? q y : q sprintf("%%%02X", z[y])
  printf("%s", q)}' "$argv"
end

function sharesheet
    if test "$TMUX" = ""
        printf "\033]"
    else
        printf "\033Ptmux;\033\033]"
    end
  
	printf "6;sharesheet://?pwd="
    printURIComponent "$PWD"
    printf "&home="
    printURIComponent "$HOME"
    if count $argv > /dev/null
      for var in $argv
        printf "&path="
        printURIComponent "$var"
      end
    else
      printf "&text="
      while read line
        printURIComponent "$line"
        echo -n %0a
      end
    end

    if test "$TMUX" = ""
        printf "\a"
    else
        printf "\a\033\\"
    end
end
