function hame-vim

    pushd $HOME
    set -l vim .vim/pack/dylan
    hame-git -n  solarized8        -p  "$vim/opt/solarized8"          -u  "lifepillar/vim-solarized8"
    hame-git -n  abolish           -p  "$vim/start/abolish"           -u  "tpope/vim-abolish"
    hame-git -n  ag                -p  "$vim/start/ag"                -u  "epmatsw/ag.vim"
    hame-git -n  commentary        -p  "$vim/start/commentary"        -u  "tpope/vim-commentary"
    hame-git -n  cpanfile          -p  "$vim/start/cpanfile"          -u  "moznion/vim-cpanfile"
    hame-git -n  endwise           -p  "$vim/start/endwise"           -u  "tpope/vim-endwise"
    hame-git -n  eunuch            -p  "$vim/start/eunuch"            -u  "tpope/vim-eunuch"
    hame-git -n  fish              -p  "$vim/start/fish"              -u  "dag/vim-fish"
    hame-git -n  fugitive          -p  "$vim/start/fugitive"          -u  "tpope/vim-fugitive"
    hame-git -n  fzf               -p  "$vim/start/fzf"               -u  "junegunn/fzf.vim"
    hame-git -n  gitgutter         -p  "$vim/start/gitgutter"         -u  "airblade/vim-gitgutter"
    hame-git -n  html5             -p  "$vim/start/html5"             -u  "othree/html5.vim"
    hame-git -n  lightline         -p  "$vim/start/lightline"         -u  "itchyny/lightline.vim"
    hame-git -n  perl              -p  "$vim/start/perl"              -u  "vim-perl/vim-perl"
    hame-git -n  racket            -p  "$vim/start/racket"            -u  "wlangstroth/vim-racket"
    hame-git -n  rails             -p  "$vim/start/rails"             -u  "tpope/vim-rails"
    hame-git -n  repeat            -p  "$vim/start/repeat"            -u  "tpope/vim-repeat"
    hame-git -n  rhubarb           -p  "$vim/start/rhubarb"           -u  "tpope/vim-rhubarb"
    hame-git -n  supertab          -p  "$vim/start/supertab"          -u  "ervandew/supertab"
    hame-git -n  surround          -p  "$vim/start/surround"          -u  "tpope/vim-surround"
    hame-git -n  syntastic         -p  "$vim/start/syntastic"         -u  "vim-syntastic/syntastic"
    hame-git -n  tabular           -p  "$vim/start/tabular"           -u  "godlygeek/tabular"
    hame-git -n  textobj-rubyblock -p  "$vim/start/textobj-rubyblock" -u  "nelstrom/vim-textobj-rubyblock"
    hame-git -n  textobj-user      -p  "$vim/start/textobj-user"      -u  "kana/vim-textobj-user"
    hame-git -n  unimpaired        -p  "$vim/start/unimpaired"        -u  "tpope/vim-unimpaired"
    hame-git -n  vim-go            -p  "$vim/start/go"                -u  "fatih/vim-go.git"
    hame-git -n  vinegar           -p  "$vim/start/vinegar"           -u  "tpope/vim-vinegar"

    set rm_packs  "$vim/start/fzy" \
                  "$vim/start/fzy-builtins" \
                  "$vim/start/fzy-find" \
                  "$vim/start/airline" \
                  "$vim/start/airline-themes"
    for pack in $rm_packs
        test -d "$pack"
        and echo "hame-vim: rm $pack"
        and rm -fr $pack
    end

    if [ -n "$HAME_HELPTAGS" ]
        for packdir in ~/.vim/pack/*
            for dir in $packdir/*
                cd $dir
                for docdir in */doc
                    hame-nq vim -u NONE -c "helptags $docdir" -c q
                end
            end
        end
    end

    popd
    return 0
end

