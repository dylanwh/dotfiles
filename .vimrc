" Dylan William Hardison's .vimrc file.
" vim: set fdm=marker:

syntax enable
filetype plugin indent on

runtime macros/matchit.vim

" OPTIONS {{{1
set tabstop=4              " number of spaces that a literal <Tab> in the file counts for.
set shiftwidth=4           " number of spaces to use for each step of (auto)indent.
set shiftround             " round indent to multiple of 'shiftwidth'.
set autoindent             " auto indent from current line to new line.
set smarttab               " insert shiftwidth or tabstop as appropriate.
set expandtab              " expand tabs
set ignorecase             " ignore case
set smartcase              " unless I use upper-case letters.
set hlsearch               " highlight searches.
set incsearch              " incremental search; highlight as you type.
set showmatch              " jump to matching brackets.
set showcmd                " show (partial) command in last line.
set secure                 " shell and write commands are not allowed in "./.vimrc" or modelines
set exrc                   " read .vimrc from current dir
set modeline               " allow modelines.
set ruler                  " use the ruler when statusline is off.
set laststatus=2           " always display a status bar.
set number                 " show line numbers.
set nocursorline           " highlight cursor line
set ttimeoutlen=50         " amount of time to wait to complete a key sequence.
set history=1000           " remember last N :commands and /searches.
set shortmess+=I           " disable splash screen
set shortmess+=T           " truncate messages
set shortmess+=s           " disable the "search hit bottom/top" messages
set cpoptions+=$           " show '$' for change operations.
set encoding=utf-8         " keep things internally as utf-8.
set fileencoding=utf-8     " read/Write files using utf-8.
set wrap                   " wrap lines
set linebreak              " break on words
set sidescroll=1           " when 'nowrap' is set, this line and the next prevent the cursor from ever reaching the prcededing char.
set sidescrolloff=1        " see above
set scrolloff=5            " always show 5 lines of context when scrolling j/k
set splitright             " split to the right
set mouse=a                " enable mouse
set cb=autoselectplus      " don't automatically put stuff in the clipboard.
set visualbell             " visual bell
set backup                 " do keep backups.
set undofile               " persistent undo
set hidden                 " allow hidden edited buffers
set autowrite              " write files if they have been modified, if we use :next, :prev, etc.
set autoread               " read in files that have changes outside of vim, if they haven't changed in vim
set bs=indent,eol,start    " backspace over identation, end-of-line, and start-of-line.
set tags+=~/.tags          " use a global tags file in $HOME
set nrformats=alpha,hex    " allow vim to increment letters and hex numbers with Ctrl-A and Ctrl-X
set viewoptions+=unix      " view files use unix EOL, even on Windows.
set viewoptions-=folds     " folds in views annoys me
set viewoptions-=options   " options in views can cause unexpected behavior.
set wim=longest,list,full  " thanks nornagon!
set grepprg=ag             " use ag for grep
set viminfo='4000          " store the last this-many marks
set viminfo+=!             " save any variables that begin with a capital letter to viminfo
set viminfo+=%             " save and restore buffer list
set viminfo+=h             " nohlsearch previous search pattern when vim starts.
set viminfo+=f1            " store all file marks
set viminfo+=r/tmp         " no viminfo for /tmp/*
set viminfo+=r/mnt         " ...
set viminfo+=r/media       " ...
set list                   " show invisible characters by default

" Ignore these filenames during enhanced command line completion.
set wildignore+=*.aux,*.out,*.toc                        " LaTeX intermediate files
set wildignore+=*.png,*.jpg,*.bmp,*.gif                  " binary images
set wildignore+=*.luac                                   " Lua byte code
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.class " compiled object files
set wildignore+=*.pyc                                    " Python byte code
set wildignore+=*.spl                                    " compiled spelling word lists
set wildignore+=*.sw?                                    " Vim swap files

set foldlevel=3
set foldopen=tag,search,quickfix,undo,jump,mark,percent
set fillchars=fold:\ ,stl:\ ,stlnc:\  " borders
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

if &termencoding ==# 'utf-8' || &encoding ==# 'utf-8'
    let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
    let &fillchars = "vert:┃,diff:⎼,fold:⎼"
endif

"}}}

" PLUGIN OPTIONS {{{1
let mapleader      = "\\"
let maplocalleader = "-"

"-- Highlight builtins.
let python_highlight_all = 1

"-- C-language settings.
let c_gnu             = 1
let c_comment_strings = 1

"-- Disable the annoying paren highlighter.
"let loaded_matchparen = 1

let perl_extended_vars           = 1 " matches hash and array subscripts, etc.
let perl_want_scope_in_variables = 1 " Shows package part of var names in green
let perl_include_pod             = 1 " Highlight POD with perl files.
let perl_string_as_statement     = 0 " Highlight quote marks different from string contents
let perl_sub_signatures          = 1 " subroutine signatures

"-- context-based supertabbing
let SuperTabDefaultCompletionType = "context"

let redcode_highlight_numbers=1

"let snippets_dir="$HOME/.vim/snippets"

let jedi#squelch_py_warning = 1

let ctrlp_extensions    = ['Z', 'F']
let ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

let airline_powerline_fonts            = 1
let airline_inactive_collapse          = 1
let airline#extensions#tabline#enabled = 1
let g:airline#themes#base16#constant = 1
let airline_theme='base16_vim'
" }}}

" COLORS {{{1
if exists('+termguicolors') && exists('$TMUX') || $TERM=='tmux-24bit'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
set termguicolors
if $ITERM_PROFILE == 'selenized-white'
    set background=light
    colorscheme selenized_bw
else
    set background=dark
    colorscheme selenized_bw
end
" }}}

" MAPPINGS {{{1
map <F1>   :set spell!<BAR>set spell?<CR>
map <F2>   :set cul!<BAR>set cul?<CR>
map <F3>   :set nu!<BAR>set nu?<CR>
map <F4>   :set nonu nocul nolist<CR>
map <F10>  :Welcome<CR>

map K \K
map Y y$

nmap <Leader>s  :shell<CR>
nmap <Leader>v  :tabedit $MYVIMRC<CR>
nmap sf         :CtrlPF<CR>
nmap sz         :CtrlPZ<CR>

nmap <Leader>> :Tab/=><CR>
vmap <Leader>> :Tab/=><CR>
nmap <Leader>= :Tab/=[^>]<CR>
vmap <Leader>= :Tab/=[^>]<CR>
nmap <Leader>: :Tab/:\zs<CR>
vmap <Leader>: :Tab/:\zs<CR>
nmap <Leader>t :Tab/
vmap <Leader>t :Tab/
nmap <Leader>w :Ag <cword><CR>

nnoremap <Left>   <C-w>h
nnoremap <Down>   <C-w>j
nnoremap <Up>     <C-w>k
nnoremap <Right>  <C-w>l

inoremap <Left>  <C-o><C-w>h
inoremap <Down>  <C-o><C-w>j
inoremap <Up>    <C-o><C-w>k
inoremap <Right> <C-o><C-w>l

" make <c-l> clear the highlight as well as redraw
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

" ABBREVS {{{1
iab  dwh Dylan William Hardison
iab <expr> dmail $EMAIL
iab  teh the
iab  hte the
iab  nad and
iab  btw by the way
" }}}

" COMMANDS {{{1
command! MakePath silent call mkdir(expand("%:p:h"), "p")
" }}}

" AUTO BOTS, TRANSFORM AND ROLL OUT {{{1
if !exists('autocmds_loaded')
    let autocmds_loaded=1
    " filetypedetect {{{2
    augroup filetypedetect
        autocmd BufNewFile,BufRead *.mkd,*.mdwn,*.md
                    \ setl ft=markdown ai formatoptions=tcroqn2 comments=n:>
        autocmd BufNewFile,BufRead *.nqp set ft=perl6
        autocmd BufNewFile,BufRead *.rem
                    \ setl ft=remind
        autocmd BufEnter *.hs,*.lhs       compiler ghc
        autocmd BufEnter *.c,*.C,*.cc,*.h compiler gcc | set et
        autocmd BufNewFile,BufRead *.csv,*.tsv set ft=csv

        autocmd BufNewFile,BufRead *.cos setl ft=caos
        autocmd BufNewFile,BufRead *.red setl ft=redcode
        autocmd BufNewFile,BufRead *.t,*.cgi setl ft=perl
        autocmd BufNewFile,BufRead *.lua setl foldmethod=marker |
                    \ setl comments=sO:-\ -,mO:-\ \ ,exO:]],s1:--[[,mb:-,ex:]],:-- |
                    \ setl commentstring=--%s
        autocmd BufNewFile,BufRead */.i3/config* set ft=i3
        autocmd BufNewFile,BufRead */.config/i3/config* set ft=i3
        autocmd BufNewFile,BufRead Capfile set ft=ruby
        autocmd BufNewFile,BufRead Vagrantfile set ft=ruby
        autocmd BufNewFile,BufRead *.tt2 set ft=tt2
        autocmd BufNewFile,BufRead *.html.tmpl set ft=tt2html
        autocmd BufNewFile,BufRead */.ssh/config.d/* set ft=sshconfig
    augroup END
    " }}}

    autocmd FileType csv       setl noet list
    autocmd FileType gitconfig setl noet nolist
    autocmd FileType gitcommit setl noet nolist
    autocmd FileType make      setl noet nolist
    autocmd FileType man       setl nolist
    autocmd FileType python    setl et
    autocmd FileType yaml      setl et ts=2 sw=2
    autocmd FileType help      setl nolist
    autocmd FileType zsh       setl path=.,~/.zsh/lib
    autocmd FileType html,javascript      setl ts=2 sw=2 et list
    autocmd FileType lua
                \ setl foldmethod=marker |
                \ setl comments=sO:-\ -,mO:-\ \ ,exO:]],s1:--[[,mb:-,ex:]],:-- |
                \ setl commentstring=--%s


    au WinLeave * if &g:cursorline |
                \ setl nocursorline |
                \ endif

    au WinEnter * if &g:cursorline |
                \ setl cursorline   |
                \ endif

    au BufWritePre /tmp/*,*/.git/COMMIT_EDITMSG setlocal noundofile
    au BufWritePost,FileWritePost */.config/i3/config.tt make -C $HOME .config/i3/config

    " set nomodifiable if the file is read-only
    autocmd BufReadPost ?*
                \ if &readonly |
                \   setlocal nomodifiable |
                \ else |
                \   setlocal modifiable |
                \ endif

endif
" }}}

if filereadable(expand("~/.vimrc.local", 1))
  source ~/.vimrc.local
endif

if filereadable(expand("~/.vim/theme.vim", 1))
  source ~/.vim/theme.vim
endif
