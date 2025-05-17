function skim
    set query "$argv"
    set results (mktemp)

    sk-tmux --ansi -i -m \
        -c 'rg --color=always --line-number \'{}\'' \
        --preview 'preview.sh {}' \
        --preview-window 'right:70%' \
        --bind '?:toggle-preview,ctrl-o:execute-silent(open {})' \
        --delimiter ':' --nth 1 --cmd-query "$query" >$results
    and emacs $results
    command rm $results
end
