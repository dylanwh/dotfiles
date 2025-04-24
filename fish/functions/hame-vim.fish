function hame-vim
    argparse "t/helptags" "U/update" "v/verbose" "h/help" -- $argv
    or return

    if test -n "$_flag_help"
        echo "Usage: hame-vim [-t] [-U] [-v] [-h]"
        echo "  -t: generate helptags"
        echo "  -U: update plugins"
        echo "  -v: verbose"
        echo "  -h: this help"
        return
    end

    set -lx HAME_VERBOSE $HAME_VERBOSE
    if [ -n "$_flag_verbose" ]
        set HAME_VERBOSE 1
    end

    pushd $HOME

    set -lx HAME_UPDATE $HAME_UPDATE
    if [ -n "$_flag_update" ]
        set HAME_UPDATE yes
        hame-echo updating vim packages
    else
        hame-echo installing vim packages
    end

    set -l vim .vim/pack/dylan
    hame-clone -n abolish           -p "$vim/start/abolish"           -u "tpope/vim-abolish"
    hame-clone -n ag                -p "$vim/start/ag"                -u "epmatsw/ag.vim"
    hame-clone -n ale               -p "$vim/start/ale"               -u "dense-analysis/ale"
    hame-clone -n applescript       -p "$vim/start/applescript"       -u "dearrrfish/vim-applescript"
    hame-clone -n commentary        -p "$vim/start/commentary"        -u "tpope/vim-commentary"
    hame-clone -n copilot           -p "$vim/start/copilot"           -u "github/copilot.vim"
    hame-clone -n cpanfile          -p "$vim/start/cpanfile"          -u "moznion/vim-cpanfile"
    hame-clone -n ctrlp             -p "$vim/start/ctrlp"             -u "ctrlpvim/ctrlp.vim"
    hame-clone -n editorconfig      -p "$vim/start/editorconfig"      -u "editorconfig/editorconfig-vim"
    hame-clone -n endwise           -p "$vim/start/endwise"           -u "tpope/vim-endwise"
    hame-clone -n eunuch            -p "$vim/start/eunuch"            -u "tpope/vim-eunuch"
    hame-clone -n starlark          -p "$vim/start/starlark"          -u "cappyzawa/starlark.vim"
    hame-clone -n exchange          -p "$vim/start/exchange"          -u "tommcdo/vim-exchange"
    hame-clone -n fish              -p "$vim/start/fish"              -u "dag/vim-fish"
    hame-clone -n fugitive          -p "$vim/start/fugitive"          -u "tpope/vim-fugitive"
    hame-clone -n gitgutter         -p "$vim/start/gitgutter"         -u "airblade/vim-gitgutter"
    hame-clone -n html5             -p "$vim/start/html5"             -u "othree/html5.vim"
    hame-clone -n jq                -p "$vim/start/jq"                -u "bfrg/vim-jq"
    hame-clone -n jsonnet           -p "$vim/start/jsonnet"           -u "google/vim-jsonnet"
    hame-clone -n lightline         -p "$vim/start/lightline"         -u "itchyny/lightline.vim"
    hame-clone -n lsp               -p "$vim/opt/lsp"                 -u "yegappan/lsp"
    hame-clone -n osc52             -p "$vim/start/osc52"             -u "fcpg/vim-osc52"
    hame-clone -n perl              -p "$vim/start/perl"              -u "vim-perl/vim-perl"
    hame-clone -n python-syntax     -p "$vim/start/python-syntax"     -u "vim-python/python-syntax"
    hame-clone -n racket            -p "$vim/start/racket"            -u "wlangstroth/vim-racket"
    hame-clone -n rails             -p "$vim/start/rails"             -u "tpope/vim-rails"
    hame-clone -n repeat            -p "$vim/start/repeat"            -u "tpope/vim-repeat"
    hame-clone -n rg                -p "$vim/start/ripgrep"           -u "jremmen/vim-ripgrep"
    hame-clone -n rhubarb           -p "$vim/start/rhubarb"           -u "tpope/vim-rhubarb"
    hame-clone -n rust              -p "$vim/start/rust"              -u "rust-lang/rust.vim"
    hame-clone -n solarized8        -p "$vim/opt/solarized8"          -u "lifepillar/vim-solarized8"
    hame-clone -n splitjoin         -p "$vim/start/splitjoin"         -u "AndrewRadev/splitjoin.vim"
    hame-clone -n supertab          -p "$vim/start/supertab"          -u "ervandew/supertab"
    hame-clone -n surround          -p "$vim/start/surround"          -u "tpope/vim-surround"
    hame-clone -n tabular           -p "$vim/start/tabular"           -u "godlygeek/tabular"
    hame-clone -n terraform         -p "$vim/start/terraform"         -u "hashivim/vim-terraform"
    hame-clone -n textobj-rubyblock -p "$vim/start/textobj-rubyblock" -u "nelstrom/vim-textobj-rubyblock"
    hame-clone -n textobj-user      -p "$vim/start/textobj-user"      -u "kana/vim-textobj-user"
    hame-clone -n unimpaired        -p "$vim/start/unimpaired"        -u "tpope/vim-unimpaired"
    hame-clone -n vim-go            -p "$vim/start/go"                -u "fatih/vim-go.git"
    hame-clone -n vinegar           -p "$vim/start/vinegar"           -u "tpope/vim-vinegar"
    hame-clone -n rust-tools        -p "$vim/start/rust-tools"        -u "simrat39/rust-tools.nvim"
    hame-clone -n lspconfig         -p "$vim/start/lspconfig"         -u "neovim/nvim-lspconfig"
    hame-clone -n groovy            -p "$vim/start/groovy"            -u "thecodesmith/vim-groovy"

    rg --no-filename -or 'Plug \'$1\'' 'url = https://github.com/(.+)' ~/.vim/pack/dylan/start/*/.git/config | sort > ~/.ideavimrc

    set rm_packs  "$vim/start/fzy" \
                  "$vim/start/fzf" \
                  "$vim/start/fzy-builtins" \
                  "$vim/start/fzy-find" \
                  "$vim/start/airline" \
                  "$vim/start/airline-themes" \
                  "$vim/start/coc" \
                  "$vim/start/syntastic"
    for pack in $rm_packs
        test -d "$pack"
        and hame-echo "hame-vim: rm $pack"
        and rm -fr $pack
    end

    if [ -n "$_flag_t" ]
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

