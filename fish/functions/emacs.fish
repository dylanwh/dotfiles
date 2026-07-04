function emacs
    if test -n "$INSIDE_EMACS"
        emacsedit --no-wait $argv
    else
        emacsedit $argv
    end
end
