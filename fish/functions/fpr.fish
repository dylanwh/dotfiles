function fpr
 gh pr view -w (gh pr list ^/dev/null | fzy | cut -f 1) ^/dev/null;
end
