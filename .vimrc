" Dylan William Hardison's .vimrc file.
" vim: set fdm=marker expandtab:

" INIT {{{

runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()

syntax on
filetype plugin indent on

runtime! ftplugin/man.vim

if exists("*mkdir")
    for dir in ["undo", "view", "backup", "swap"]
        try
            call mkdir($XDG_CACHE_HOME . "/vim/" . dir, "p")
        catch /^Vim\%((\a\+)\)\=:E739/
            " do nothing
        endtry
    endfor
endif

" }}}

" OPTIONS {{{
set tabstop=4          " Number of spaces that a literal <Tab> in the file counts for.
set shiftwidth=4       " Number of spaces to use for each step of (auto)indent.
set shiftround         " Round indent to multiple of 'shiftwidth'.
set autoindent         " Auto indent from current line to new line.
set smarttab           " Insert shiftwidth or tabstop as appropriate.
set expandtab          " expand tabs
set ignorecase         " Ignore case
set smartcase          " Unless I use upper-case letters.
set showmatch          " Show matching brackets.
set showcmd            " show (partial) command in last line.
set hlsearch           " Highlight searches.
set incsearch          " Incremental search; highlight as you type.
set secure             " shell and write commands are not allowed in "./.vimrc".
set exrc               " Read .vimrc from current dir (off)
set modeline           " Allow modelines.
set ruler              " Show cursor position at all times.
set laststatus=2       " Always display a status bar.
set history=1000       " Remember last N :commands and /searches.
set showbreak=+\       " Prefixed wrapped lines with "+ ".
set shortmess+=I       " Disable splash screen
set shortmess+=T       " Truncate messages
set shortmess+=s       " Disable the "search hit bottom/top" messages
set cpoptions+=$       " Show '$' for change operations.
set encoding=utf-8     " Keep things internally as utf-8.
set fileencoding=utf-8 " Read/Write files using utf-8.
set hidden             " allow hidden edited buffers
set nowrap
set sidescroll=5       "
set mouse=             " disable mouse
set clipboard=         " don't automatically put stuff in the clipboard.
set vb                 " visual bell
set backup             " Do keep backups.
set undofile           " persistent undo
set autowrite          " write files if they have been modified, if we use :next, :prev, etc.
set autoread           " read in files that have changes outside of vim, if they haven't changed in vim
set cursorline         " highlight cursor line
set number
set backspace=indent,eol,start
set tags+=~/.tags,.tags
set nrformats=alpha,hex
set fileformats=unix,dos,mac
set viewoptions=cursor,folds,slash,unix
set wildmode=longest,list,full " thanks nornagon!
set wildignore=*.bak,~,*.o,*.info,*.swp,*.dvi,*.pdf,.*
set grepprg=ag

set directory=$XDG_CACHE_HOME/vim/swap//
set backupdir=$XDG_CACHE_HOME/vim/backup//
set undodir=$XDG_CACHE_HOME/vim/undo//
set viewdir=$XDG_CACHE_HOME/vim/view
set viminfo=!,'1000,%,h,f1,n$XDG_CACHE_HOME/vim/info

set foldopen=tag,search,quickfix,undo,jump,mark,percent
set fillchars=fold:\ ,stl:\ ,stlnc:\  " borders

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
    let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
endif

"}}}

" {{{ PLUGIN OPTIONS
let mapleader      = "-"
let maplocalleader = "_"

"-- Highlight builtins.
let python_highlight_all = 1

"-- C-language settings.
let c_gnu=1
let c_comment_strings=1

"-- Disable the annoying paren highlighter.
"let loaded_matchparen = 1

let vimwiki_list = [{'path': "~/.local/Dropbox/Documents/wiki"}]
let vimwiki_hl_cb_checked = 1

"-- matches hash and array subscripts, etc.
let perl_extended_vars = 1

"-- Shows package part of var names in green
let perl_want_scope_in_variables = 1

"-- Highlight POD with perl files.
let perl_include_pod = 1

"-- Highlight quotes as a statement (orange),
"-- different from string contents.
let perl_string_as_statement = 1

"-- context-based supertabbing
let SuperTabDefaultCompletionType = "context"
let SuperTabCompletionContexts = ['s:ContextText', 's:ContextDiscover']
let SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']
let SuperTabContextDiscoverDiscovery =
    \ ["&completefunc:<c-x><c-u>", "&omnifunc:<c-x><c-o>"]

let redcode_highlight_numbers=1

let solarized_bold = 0
let solarized_underline = 0
let solarized_italic = 1

let snippets_dir="$HOME/.vim/snippets"

" NERDTree config
let NERDTreeChDirMode=2
"let NERDTreeIgnore=['\env','\.vim$', '\~$', '\.pyc$', '\.swp$', '\.egg-info$', '^dist$', '^build$']
"let NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\~$']
let NERDTreeShowBookmarks=1
let NERDTreeHightlightCursorline=1
let NERDTreeBookmarksFile=".NERDTreeBookmarks"

let g:jedi#squelch_py_warning = 1

let g:ctrlp_z_nerdtree = 1

let g:ctrlp_extensions = ['Z', 'F']

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

" }}}

" {{{ COLORS
set background=dark
colorscheme solarized

" highlight StatusLine   ctermfg=7  ctermbg=0    guibg=#002b36 gui=none
" highlight StatusLine   ctermfg=10 ctermbg=0    guibg=#002b36 gui=none

" highlight UserFile     ctermfg=4  ctermbg=0    guifg=#268bd2 guibg=#002b36 gui=none
" highlight UserFile     ctermfg=4  ctermbg=0    guifg=#268bd2 guibg=#002b36 gui=none
" highlight UserGit      ctermfg=2  ctermbg=0    guifg=#859900 guibg=#002b36 gui=none
" highlight UserWarn     ctermfg=9  ctermbg=0    guifg=#cb4b16 guibg=#002b36 gui=none
" highlight UserMisc     ctermfg=10 ctermbg=0    guifg=#586e75 guibg=#002b36 gui=none
" highlight UserNotice   ctermfg=7  ctermbg=0    guifg=#eee8d5 guibg=#002b36 gui=none
" highlight UserFT       ctermfg=3  ctermbg=0    guifg=#b58900 guibg=#002b36 gui=none
" }}}

" MAPPINGS {{{
map <F1> :set spell!<BAR>set spell?<CR>
map <F2> :set cul!<BAR>set cul?<CR>
map <F3> :set nu!<BAR>set nu?<CR>
map <F4> :set nonu nocul<BAR>set nu? cul?<CR>

map gn <C-o>:tab new<CR>
map K \K
map <c-w><c-t> :WMToggle<cr>
map <c-w><c-f> :FirstExplorerWindow<cr>
map <c-w><c-b> :BottomExplorerWindow<cr>
map Y y$

nmap <silent><Leader>wf <Plug>VimwikiFollowWord
nmap <silent><Leader>wb <Plug>VimwikiGoBackWord
nmap <silent><Leader>wn <Plug>VimwikiGoBackWord

nmap <Leader>t :NERDTreeToggle<cr>
nmap <Leader>n :NERDTreeFocus<cr>
nmap <Leader>f :NERDTreeFind<cr>
nmap <Leader>s  :shell<cr>
nmap <Leader>e  :CtrlPF<cr>
nmap <Leader>d  :CtrlPZ<cr>
nmap sf         :CtrlPF<cr>
nmap sz         :CtrlPZ<cr>


nnoremap <Left> <C-w>h
nnoremap <Down> <C-w>j
nnoremap <Up>   <C-w>k
nnoremap <Right> <C-w>l

inoremap <Left> <C-o><C-w>h
inoremap <Down> <C-o><C-w>j
inoremap <Up>   <C-o><C-w>k
inoremap <Right> <C-o><C-w>l

"make <c-l> clear the highlight as well as redraw
nnoremap <C-L> :nohls<CR><C-L>
inoremap <C-L> <C-O>:nohls<CR>

imap <C-w>h  <C-o><C-w>h
imap <C-w>j  <C-o><C-w>j
imap <C-w>k  <C-o><C-w>k
imap <C-w>l  <C-o><C-w>l
imap <C-w>_  <C-o><C-w>_
imap <C-w>\| <C-o><C-w>\|
imap <C-w>=  <C-o><C-w>=
" }}}

" ABBREVS {{{
iab  dwh Dylan William Hardison
iab <expr> dmail $EMAIL
iab  teh the
iab  hte the
iab  nad and
iab  btw by the way
" }}}

" COMMANDS {{{
command MakePath silent call mkdir(expand("%:p:h"), "p")
" }}}

" AUTO BOTS, TRANSFORM AND ROLL OUT {{{
if !exists('autocmds_loaded')
    let autocmds_loaded=1
    " filetypedetect {{{
    augroup filetypedetect
        autocmd BufNewFile,BufRead *.mkd,*.mdwn,*.md
                    \ setl ft=markdown ai formatoptions=tcroqn2 comments=n:>
        autocmd BufNewFile,BufRead *.rem
                    \ setl ft=remind
        autocmd BufEnter *.hs,*.lhs       compiler ghc
        autocmd BufEnter *.c,*.C,*.cc,*.h compiler gcc
        autocmd BufNewFile,BufRead *.csv,*.tsv set ft=csv

        autocmd BufNewFile,BufRead *.cos setl ft=caos
        autocmd BufNewFile,BufRead *.red setl ft=redcode
        autocmd BufNewFile,BufRead *.t,*.cgi setl ft=perl
        autocmd BufNewFile,BufRead *.lua setl foldmethod=marker |
                    \ setl comments=sO:-\ -,mO:-\ \ ,exO:]],s1:--[[,mb:-,ex:]],:-- |
                    \ setl commentstring=--%s
        autocmd BufNewFile,BufRead */.i3/config* set ft=i3
        autocmd BufNewFile,BufRead */.config/i3/config* set ft=i3
    augroup END
    " }}}

    autocmd FileType csv setl noet list
    autocmd FileType gitconfig setl noet nolist
    autocmd FileType gitcommit setl noet nolist
    autocmd FileType make setl noet nolist
    autocmd FileType man  setl nolist
    autocmd FileType python setl et
    autocmd FileType yaml   setl et

    au WinLeave * set nocursorline
    au WinEnter * set cursorline

    " set nomodifiable if the file is read-only
    autocmd BufReadPost ?*
                \ if &readonly |
                \   setlocal nomodifiable |
                \ else |
                \   setlocal modifiable |
                \ endif
endif
" }}}

"if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
"ndif
