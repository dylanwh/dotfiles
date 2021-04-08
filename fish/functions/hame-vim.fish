function hame-vim

    pushd $HOME
    set -l vim .vim/pack/dylan
    hame-clone -n solarized8        -p "$vim/opt/solarized8"          -u "lifepillar/vim-solarized8"
    hame-clone -n abolish           -p "$vim/start/abolish"           -u "tpope/vim-abolish"
    hame-clone -n ag                -p "$vim/start/ag"                -u "epmatsw/ag.vim"
    hame-clone -n applescript       -p "$vim/start/applescript"       -u "dearrrfish/vim-applescript"
    hame-clone -n commentary        -p "$vim/start/commentary"        -u "tpope/vim-commentary"
    hame-clone -n cpanfile          -p "$vim/start/cpanfile"          -u "moznion/vim-cpanfile"
    hame-clone -n endwise           -p "$vim/start/endwise"           -u "tpope/vim-endwise"
    hame-clone -n eunuch            -p "$vim/start/eunuch"            -u "tpope/vim-eunuch"
    hame-clone -n fish              -p "$vim/start/fish"              -u "dag/vim-fish"
    hame-clone -n fugitive          -p "$vim/start/fugitive"          -u "tpope/vim-fugitive"
    hame-clone -n fzf               -p "$vim/start/fzf"               -u "junegunn/fzf.vim"
    hame-clone -n gitgutter         -p "$vim/start/gitgutter"         -u "airblade/vim-gitgutter"
    hame-clone -n html5             -p "$vim/start/html5"             -u "othree/html5.vim"
    hame-clone -n jq                -p "$vim/start/jq"                -u "bfrg/vim-jq"
    hame-clone -n jsonnet           -p "$vim/start/jsonnet"           -u "google/vim-jsonnet"
    hame-clone -n lightline         -p "$vim/start/lightline"         -u "itchyny/lightline.vim"
    hame-clone -n osc52             -p "$vim/start/osc52"             -u "fcpg/vim-osc52"
    hame-clone -n perl              -p "$vim/start/perl"              -u "vim-perl/vim-perl"
    hame-clone -n racket            -p "$vim/start/racket"            -u "wlangstroth/vim-racket"
    hame-clone -n rails             -p "$vim/start/rails"             -u "tpope/vim-rails"
    hame-clone -n repeat            -p "$vim/start/repeat"            -u "tpope/vim-repeat"
    hame-clone -n rhubarb           -p "$vim/start/rhubarb"           -u "tpope/vim-rhubarb"
    hame-clone -n supertab          -p "$vim/start/supertab"          -u "ervandew/supertab"
    hame-clone -n surround          -p "$vim/start/surround"          -u "tpope/vim-surround"
    hame-clone -n syntastic         -p "$vim/start/syntastic"         -u "vim-syntastic/syntastic"
    hame-clone -n tabular           -p "$vim/start/tabular"           -u "godlygeek/tabular"
    hame-clone -n textobj-rubyblock -p "$vim/start/textobj-rubyblock" -u "nelstrom/vim-textobj-rubyblock"
    hame-clone -n textobj-user      -p "$vim/start/textobj-user"      -u "kana/vim-textobj-user"
    hame-clone -n unimpaired        -p "$vim/start/unimpaired"        -u "tpope/vim-unimpaired"
    hame-clone -n vim-go            -p "$vim/start/go"                -u "fatih/vim-go.git"
    hame-clone -n vinegar           -p "$vim/start/vinegar"           -u "tpope/vim-vinegar"

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

