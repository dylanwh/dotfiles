function hame-emacs
    pushd $HOME
    if [ -d .emacs.d ]
        if command git -C .emacs.d remote -v | grep -q spacemacs
            mv .emacs.d .emacs.spacemacs
        end
    end

    hame-echo cloning doom-emacs
    hame-clone -n doom-emacs -p .emacs.d -u 'hlissner/doom-emacs'
    user-path add ~/.emacs.d/bin
    if not [ -d .emacs.d/.local ]
        hame-echo installing doom-emacs
        hame-nq bash -c "doom install --force --env --no-fonts"
    end
    popd
end

