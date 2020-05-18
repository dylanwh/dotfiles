function hame
    argparse "f/force" -- $argv

    set -l vim .vim/pack/dylan
    set -l f ""
    if [ $_flag_force ]
        set f "--force"
    end

    hame-git $f -n  solarized8        -p "$vim/opt/solarized8"          -u  "lifepillar/vim-solarized8"
    hame-git $f -n  abolish           -p "$vim/start/abolish"           -u  "tpope/vim-abolish"
    hame-git $f -n  ag                -p "$vim/start/ag"                -u  "epmatsw/ag.vim"
    hame-git $f -n  airline           -p "$vim/start/airline"           -u  "vim-airline/vim-airline"
    hame-git $f -n  airline-themes    -p "$vim/start/airline-themes"    -u  "vim-airline/vim-airline-themes"
    hame-git $f -n  commentary        -p "$vim/start/commentary"        -u  "tpope/vim-commentary"
    hame-git $f -n  cpanfile          -p "$vim/start/cpanfile"          -u  "moznion/vim-cpanfile"
    hame-git $f -n  endwise           -p "$vim/start/endwise"           -u  "tpope/vim-endwise"
    hame-git $f -n  eunuch            -p "$vim/start/eunuch"            -u  "tpope/vim-eunuch"
    hame-git $f -n  fish              -p "$vim/start/fish"              -u  "dag/vim-fish"
    hame-git $f -n  fugitive          -p "$vim/start/fugitive"          -u  "tpope/vim-fugitive"
    hame-git $f -n  gitgutter         -p "$vim/start/gitgutter"         -u  "airblade/vim-gitgutter"
    hame-git $f -n  html5             -p "$vim/start/html5"             -u  "othree/html5.vim"
    hame-git $f -n  perl              -p "$vim/start/perl"              -u  "vim-perl/vim-perl"
    hame-git $f -n  racket            -p "$vim/start/racket"            -u  "wlangstroth/vim-racket"
    hame-git $f -n  rails             -p "$vim/start/rails"             -u  "tpope/vim-rails"
    hame-git $f -n  repeat            -p "$vim/start/repeat"            -u  "tpope/vim-repeat"
    hame-git $f -n  supertab          -p "$vim/start/supertab"          -u  "ervandew/supertab"
    hame-git $f -n  surround          -p "$vim/start/surround"          -u  "tpope/vim-surround"
    hame-git $f -n  syntastic         -p "$vim/start/syntastic"         -u  "vim-syntastic/syntastic"
    hame-git $f -n  tabular           -p "$vim/start/tabular"           -u  "godlygeek/tabular"
    hame-git $f -n  textobj-rubyblock -p "$vim/start/textobj-rubyblock" -u  "nelstrom/vim-textobj-rubyblock"
    hame-git $f -n  textobj-user      -p "$vim/start/textobj-user"      -u  "kana/vim-textobj-user"
    hame-git $f -n  unimpaired        -p "$vim/start/unimpaired"        -u  "tpope/vim-unimpaired"
    hame-git $f -n  vinegar           -p "$vim/start/vinegar"           -u  "tpope/vim-vinegar"
end

