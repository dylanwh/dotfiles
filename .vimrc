" Dylan William Hardison's .vimrc file.
" vim: set fdm=marker expandtab:
if has("terminfo")
    if &t_Co < 16
        set t_Co=16
        set t_AB=[%?%p1%{8}%<%t%p1%{40}%+%e%p1%{92}%+%;%dm
        set t_AF=[%?%p1%{8}%<%t%p1%{30}%+%e%p1%{82}%+%;%dm
    end
else
  set t_Co=16
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif

syntax on
filetype plugin on
filetype indent on

runtime! ftplugin/man.vim

if $TERM == 'rxvt-unicode-outside'
    set bg=light
    colorscheme default
else
    colorscheme dylan2
endif

" OPTIONS {{{
set tabstop=4          " Number of spaces that a literal <Tab> in the file counts for.
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
set exrc             " Read .vimrc from current dir (off)
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
set title
set wildmode=longest,list,full " thanks nornagon!
set wildignore=*.bak,~,*.o,*.info,*.swp,*.dvi,*.pdf,.*
set backspace=eol,start,indent
set grepprg=grep\ -nH\ \ --exclude='*.svn*'\ $*
set foldopen=tag,search,quickfix,undo,jump,mark,percent
set viminfo=!,'1000,%,h,f1,n~/.viminfo
set statusline=%<%f\ %h%m%r%{FF()}%{FENC()}%y%=0x%B\ %-14.(%l,%c%V%)\ %P
set fillchars=fold:\ ,stl:\ ,stlnc:\  " borders
set tags+=~/.tags,.tags
set nowrap
set sidescroll=5
set listchars=tab:>.,trail:_,precedes:<,extends:>
set nolist  " This and the above line make for visible whitespace.
set mouse=  " disable mouse

let mapleader = "\\"
let maplocalleader = ","

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

let g:vimwiki_list = [{'path': "~/.local/Dropbox/Documents/wiki"}]
let g:vimwiki_hl_cb_checked = 1

let autodate_format = "%Y-%m-%d"

"-- matches hash and array subscripts, etc.
let perl_extended_vars = 1

"-- Shows package part of var names in green
let perl_want_scope_in_variables = 1

"-- Highlight POD with perl files.
let perl_include_pod = 1

"-- Highlight quotes as a statement (orange),
"-- different from string contents.
let perl_string_as_statement = 1

"-- enable perl folding
let perl_fold = 1

let redcode_highlight_numbers=1

" }}}

" MAPPINGS {{{
map <F1> :tab help<CR>
map <F2> :nohlsearch<CR>
map <F3> :set nu!<BAR>set nu?<CR>
map <F4> :%s/\s\+$//<CR>

map gn <C-o>:tab new<CR>
map K \K
map <c-w><c-t> :WMToggle<cr>
map <c-w><c-f> :FirstExplorerWindow<cr>
map <c-w><c-b> :BottomExplorerWindow<cr>

noremap <Space> <PageDown>
noremap -       <PageUp>

nmap <silent><Leader>wf <Plug>VimwikiFollowWord
nmap <silent><Leader>wb <Plug>VimwikiGoBackWord
nmap <silent><Leader>wn <Plug>VimwikiGoBackWord
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

" FUNCTIONS {{{
function! FF()
    let val = "[" . &ff . "]"
    if val != "[unix]"
        return val
    else
        return ""
    endif
endfunction

function! FENC()
    let val = "[" . &fenc . "]"
    if val != "[utf-8]"
        return val
    else
        return ""
    endif
endfunction

function! NativeTraits() range
    if match(getline(a:firstline), "has") == -1
        throw "This does not look like a has block"
    endif

    let range_has = a:firstline . "," . a:lastline
    exec range_has . "sm/metaclass.*=>.*Collection::\\(\\w\\+\\).*,/traits => ['\\1'],/"

    call cursor(a:firstline, 0)
    let pfirst = search('provides\s*=>\s*{', "nW", a:lastline)

    call cursor(pfirst, 0)
    let plast = search('^\s*}', "nW", a:lastline)
    let range_prov = pfirst . "," . plast

    exec range_prov . "sm/\\(\\w\\+\\)\\s*=>\\s*'\\(\\w\\+\\)'/\\2 => '\\1'/"
    exec range_prov . "s/provides/handles/"
endfunction

" }}}

" COMMANDS {{{
command MakePath silent call mkdir(expand("%:p:h"), "p")
command -range NativeTraits :<line1>,<line2>call NativeTraits()
" }}}

" AUTO BOTS, TRANSFORM AND ROLL OUT {{{
if !exists('autocmds_loaded')
    let autocmds_loaded=1
    augroup filetypedetect
        autocmd BufNewFile,BufReadPost *.tt,*.ttml,*.html,*.tt2
                    \ setl ft=html syn=template
        autocmd BufNewFile,BufRead *.mkd,*.mdwn
                    \ setl ai formatoptions=tcroqn2 comments=n:>
        autocmd BufNewFile,BufRead *.rem
                    \ setl ft=remind
        autocmd BufEnter *.hs,*.lhs       compiler ghc
        autocmd BufEnter *.c,*.C,*.cc,*.h compiler gcc

        autocmd BufNewFile,BufRead *.cos setl ft=caos
        autocmd BufNewFile,BufRead *.red setl ft=redcode
        autocmd BufNewFile,BufRead *.t,*.cgi setl ft=perl
        autocmd BufNewFile,BufRead *.lua setl foldmethod=marker |
                    \ setl comments=sO:-\ -,mO:-\ \ ,exO:]],s1:--[[,mb:-,ex:]],:-- |
                    \ setl commentstring=--%s
    augroup END

    autocmd FileType gitconfig setl noet nolist
    autocmd FileType gitcommit setl noet nolist
    autocmd FileType make setl noet nolist
    autocmd FileType man  setl nolist
    autocmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \   exe "normal g`\"" |
                \ endif
endif
" }}}
