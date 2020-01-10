have broot
and function br
    set f (mktemp)
    broot --outcmd $f $argv
    if test $status -ne 0
        rm -f "$f"
        return "$code"
    end
    set d (cat "$f")
    rm -f "$f"
    eval "$d"
end
or function br
    echo "broot is not installed."
end
