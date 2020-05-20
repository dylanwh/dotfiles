function fco
    set fmt '%(committerdate:short) | %(refname:short) | %(contents:subject)'
    set git_args --sort=-committerdate \
                refs/heads/ \
                --format=$fmt
    set branch (
        git for-each-ref $git_args \
        | column -s '|' -t \
        | fzy \
        | awk '{print $2}' \
    )
    or return
    git switch $branch
end
