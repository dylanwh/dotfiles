"          FILE: xdg_dirs.vim
"      Language: vim script
"    Maintainer: Dylan William Hardison <dylan@hardison.net>
"       Created: 2013-05-20 12:00:27
" Last Modified: 2013-05-20 12:00:27
"   Description: 
"       This is a simple script to make vim use some (or all, optionally) of
"       the XDG directories (XDG_CONFIG_HOME, XDG_DATA_HOME, XDG_CACHE_HOME)

if exists("g:loaded_xdg_dirs")
    finish
endif

let g:loaded_xdg_dirs = 1

" XDG_*HOME default values {{{
if empty($XDG_CACHE_HOME)
    let $XDG_CACHE_HOME=$HOME . "/.cache"
endif

if empty($XDG_CONFIG_HOME)
    let $XDG_CONFIG_HOME=$HOME . "/.config"
endif

if empty($XDG_DATA_HOME)
    let $XDG_DATA_HOME=$HOME . "/.local/share"
endif
" }}}

function s:MakePath(path) "{{{
    let path = substitute(a:path, '//$', "", "g")
    if exists("*mkdir")
        try
            call mkdir(path, "p")
        catch /^Vim\%((\a\+)\)\=:E739/
            " do nothing
        endtry
    else
        let cmd = "mkdir -p " . shellescape(path)
        call system(cmd)
    endif
endfunction "}}}

if has("nvim")
    set directory=$XDG_DATA_HOME/nvim/swap//
    set backupdir=$XDG_DATA_HOME/nvim/backup//
    set undodir=$XDG_DATA_HOME/nvim/undo//
    set viewdir=$XDG_DATA_HOME/nvim/view
    set viminfo+=n$XDG_DATA_HOME/nvim/info
else
    set directory=$XDG_DATA_HOME/vim/swap//
    set backupdir=$XDG_DATA_HOME/vim/backup//
    set undodir=$XDG_DATA_HOME/vim/undo//
    set viewdir=$XDG_DATA_HOME/vim/view
    set viminfo+=n$XDG_DATA_HOME/vim/info
endif

let g:netrw_home = $XDG_DATA_HOME . "/vim/netrw"

call s:MakePath(&directory)
call s:MakePath(&backupdir)
call s:MakePath(&undodir)
call s:MakePath(&viewdir)
call s:MakePath(g:netrw_home)

" vim: set fdm=marker:
