function hame-emacs
    pushd $HOME
    if [ -d .emacs.d ]
        if git -C .emacs.d remote -v | grep -q spacemacs
            mv .emacs.d .emacs.spacemacs
        end
    end

    hame-git -n doom-emacs -p .emacs.d -u 'hlissner/doom-emacs'
    path add ~/.emacs.d/bin
    if not [ -d .emacs.d/.local ]
        hame-nq doom -y install --no-fonts
    end
    popd
end

