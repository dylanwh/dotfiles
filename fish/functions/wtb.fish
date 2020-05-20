function wtb -a branch
    if not test -e .git
        echo "wtb must be run from a dir with a .git file or directory" >&2
        return 1
    end

    if test -z "$branch"
        echo "Usage: wtb branch-name" >&2
        return 1
    end

    set -l prefix (git repo-name)
    set -l dir "../$prefix-$branch"

    if test -d "$dir"
        echo "directory $dir already exists" >&2
        return 1
    end

    git branch dylan/$branch
    or return 1

    git worktree add $dir dylan/$branch
    or return 1

    cd $dir
    or return 1
end
