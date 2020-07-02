function sd
    set dir (sk --ansi -i \
                -c 'fd --color=always -t d {}' \
                --preview-window right:70%   \
                --bind '?:toggle-preview,ctrl-o:execute-silent(open {})' \
                --preview 'exa --color=always -hlF --git --group-directories-first -T -L2 {}')
    or return 1
    cd "$dir"
end
