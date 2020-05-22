function hame-emacs
    if [ -d .emacs.d ]
        pushd .emacs.d
        if git remote -v | grep -q spacemacs
            popd
            mv .emacs.d .emacs.spacemacs
        else
            popd
        end
    end

    hame-git -n doom-emacs -p .emacs.d -u 'hlissner/doom-emacs'
    path add ~/.emacs.d/bin
    if not [ -d .emacs.d/.local ]
        hame-nq doom -y install --no-fonts
    end
end

