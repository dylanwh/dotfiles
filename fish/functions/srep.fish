function srep
 sk --ansi -i -c 'rg --color=always --line-number "{}"' --preview 'bat --color always -r {2}: {1}; or exa --color always {1}'  --delimiter ':' --nth 1 --cmd-query "$argv[1]";
end
