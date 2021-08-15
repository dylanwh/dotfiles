function fpr
 gh pr view -w (gh pr list 2>/dev/null | fzy | cut -f 1) 2>/dev/null;
end
