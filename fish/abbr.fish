abbr ag rg
abbr grep rg
abbr yo yoink
abbr rm 'rm -i'
abbr mv 'mv -i'
abbr cp 'cp -i'
abbr ci 'git commit'
abbr gap 'git add -p'
abbr gco 'git checkout'
abbr gdc 'git diff --cached'
abbr gr 'git rebase'
abbr gs 'git status --short'
abbr pt 'perltidy --profile=.../.perltidyrc -b  -bext=/'
abbr pull 'git pull'
abbr push 'git push'
abbr runti 'docker run --rm -ti'
abbr upstream 'git push --set-upstream origin (git branch --show-current)'
abbr hm history merge

set -l ver (string split . $FISH_VERSION)
# fish 3.6.0 or later
if [ $ver[1] -ge 3 -a $ver[2] -ge 6 ]
    function gh_abbr -a prefix
        set match (string match -g -r 'gh:([^/]+)/(.*)' $prefix)
        set user $match[1]
        set repo $match[2]
        echo "git@github.com:$user/$repo.git"
    end

    function vim_edit
        echo vim $argv
    end

    abbr -a vim_edit_texts --position command --regex ".+\.txt" --function vim_edit

    abbr -a github_prefix \
        --position anywhere \
        --regex 'gh:[^/]+/[^/]+' \
        --function gh_abbr

    function last_history_item
        echo $history[1]
    end
    abbr -a !! --position anywhere --function last_history_item
end
