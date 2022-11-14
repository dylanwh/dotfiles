# Defined in /tmp/fish.jGVBOj/skim.fish @ line 2
function skim
    set query "$argv"
    set results (mktemp)

    have vim
    and set -l vim vim
    have nvim
    and set -l vim nvim

    sk-tmux --ansi -i -m \
        -c 'rg --color=always --line-number \'{}\'' \
        --preview 'preview.sh {}' \
        --preview-window 'right:70%' \
        --bind '?:toggle-preview,ctrl-o:execute-silent(open {})' \
        --delimiter ':' --nth 1 --cmd-query "$query" > $results
    and $vim -q $results
    command rm $results
end
