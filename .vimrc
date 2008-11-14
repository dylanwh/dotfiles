" Dylan William Hardison's .vimrc file.
" vim: set fdm=marker expandtab:
syntax on
filetype plugin on
filetype indent on

" OPTIONS {{{
set tabstop=4          " Number of spaces that a <Tab> in the file counts for. 
set shiftwidth=4       " Number of spaces to use for each step of (auto)indent. 
set shiftround         " Round indent to multiple of 'shiftwidth'.
set autoindent         " Auto indent from current line to new line.
set copyindent         " Try to not change the indentation style.
set preserveindent     " Ditto.
set smarttab           " Insert shiftwidth or tabstop as appropriate.
set ignorecase         " Ignore case
set smartcase          " Unless I use upper-case letters.
set showmatch          " Show matching brackets.
set showcmd            " show (partial) command in last line.
set nobackup           " Do not keep backups.
set hlsearch           " Highlight searches.
set incsearch          " Incremental search; highlight as you type.
set secure             " shell and write commands are not allowed in "./.vimrc".
set exrc               " Read .vimrc from current dir
set modeline           " Allow modelines.
set ruler              " Show cursor position at all times.
set laststatus=2       " Always display a status bar.
set history=1000       " Remember last N :commands and /searches.
set showbreak=+\       " Prefixed wrapped lines with "+ ".
set shortmess=Ia       " Disable splash screen
set cpoptions+=$       " Show '$' for change operations.
set encoding=utf-8     " Keep things internally as utf-8.
set fileencoding=utf-8 " Read/Write files using utf-8.
set titlestring=vim:\ %F
set wildmode=longest,list,full " thanks nornagon!
set wildignore=*.bak,~,*.o,*.info,*.swp,*.dvi,*.pdf,.*
set backspace=eol,start,indent
set grepprg=grep\ -nH\ \ --exclude='*.svn*'\ $*
set foldopen=tag,search,quickfix,undo,jump,mark,percent
set viminfo=!,'1000,%,h,f1,n~/.viminfo
set statusline=%<%f\ %h%m%r%{FF()}%y%=0x%b\ %-14.(%l,%c%V%)\ %P
set fillchars=fold:\ ,stl:\ ,stlnc:\  
set tags+=~/.tags,.tags

"let mapleader = "\\"
"let maplocalleader = ",,"

"-- My name for adding chanelog entries.
let changelog_username = $REALNAME " <".$EMAIL.">"

"-- My name for adding debian changelog entries.
let debianfullname = $REALNAME 

"-- Highlight builtins.
let python_highlight_all = 1

"-- gnu!
let c_gnu=1
let c_comment_strings=1

"-- Disable the annoying paren highlighter.
let loaded_matchparen = 1
let g:inkpot_black_background = 1
let g:bluez_transbg = 1

"-- Chicken Scheme!
let is_chicken = 1

let g:haddock_browser = "sensible-browser"

let g:git_branch_status_head_current=1 
let g:git_branch_status_text="[git"
let g:git_branch_status_around=":]"

let g:vimwiki_home = "~/docs/wiki/" 
let g:vimwiki_smartCR = 1

let autodate_format = "%Y-%m-%d"
" }}}

" MAPPINGS {{{
map <F1> <C-o>:tab help<CR>
map <F2> <C-o>:nohlsearch<CR>
map <F3> <C-o>:set nu!<BAR>set nu?<CR>
map <F4> <C-o>:edit ~/.menu<CR>
map gn <C-o>:tab new<CR>
map K \K
map <c-w><c-t> :WMToggle<cr>
map <c-w><c-f> :FirstExplorerWindow<cr>
map <c-w><c-b> :BottomExplorerWindow<cr>

noremap <Space> <PageDown>
noremap -       <PageUp>

nmap ,,f :Explore<CR>
nmap <Up>   gk
nmap <Down> gj

imap <C-w>j  <C-o><C-w>j
imap <C-w>k  <C-o><C-w>k
imap <C-w>l  <C-o><C-w>l
imap <C-w>h  <C-o><C-w>h
imap <C-w>_  <C-o><C-w>_
imap <C-w>\| <C-o><C-w>\|
imap <C-w>=  <C-o><C-w>=
imap <Up>    <C-o>gk
imap <Down>  <C-o>gj
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
    augroup filetypedetect
        autocmd BufNewFile,BufReadPost *.tt,*.ttml,*.html
                    \ setl ft=html syn=template
        autocmd BufNewFile,BufReadPost *.ttex
                    \ setl syn=template-tex
        autocmd BufNewFile,BufReadPost *.hs,*.lhs,*.hsc 
                    \ setl noshiftround noexpandtab ts=8 sw=4
        autocmd BufNewFile,BufRead *.mkd,*.mdwn           
                    \ setl ai formatoptions=tcroqn2 comments=n:>
        autocmd BufNewFile,BufRead *.rem
                    \ setl ft=remind
        autocmd BufEnter *.hs,*.lhs       compiler ghc
        autocmd BufEnter *.c,*.C,*.cc,*.h compiler gcc
        autocmd BufNewFile,BufRead *.cos set ft=caos
        autocmd BufNewFile,BufRead *.lua set foldmethod=marker |
                    \ set comments=sO:-\ -,mO:-\ \ ,exO:]],s1:--[[,mb:-,ex:]],:-- |
                    \ set commentstring=--%s
    augroup END
    autocmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \   exe "normal g`\"" |
                \ endif

endif
" }}}

runtime ftplugin/man.vim
colorscheme dylan2

function FF()
    let val = "[" . &ff . "]"
    if val != "[unix]"
        return val
    else
        return ""
    endif
endfunction

    
