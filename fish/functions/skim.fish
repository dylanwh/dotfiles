#!/usr/bin/env fish

function skim
    set query "$argv"
    set results (mktemp)
    sk --ansi -i -m \
        -c 'rg --color=always --line-number \'{}\'' \
        --preview 'preview.sh {}' \
        --preview-window 'right:70%' \
        --bind '?:toggle-preview,ctrl-o:execute-silent(open {})' \
        --delimiter ':' --nth 1 --cmd-query "$query" > $results
    and vim -q $results
    command rm $results
end
