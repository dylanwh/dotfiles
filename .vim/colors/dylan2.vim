" Vim color file
" Maintainer:   Dylan Hardison <dylanwh@gmail.com>
" Last Change:  2010-02-13.

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
    syntax reset
endif

let colors_name = "dylan2"
set bg=dark

hi StatusLine   cterm=none ctermbg=4
hi StatusLineNC cterm=none ctermbg=4 ctermfg=darkgray
hi TabLine      ctermbg=4 ctermfg=7 cterm=none
hi TabLineFill  ctermbg=4 cterm=none
hi VertSplit    term=none cterm=none ctermbg=4
hi Folded       cterm=bold ctermbg=none ctermfg=5
hi Directory    cterm=bold ctermfg=4
hi Normal       cterm=none ctermfg=7
hi Comment      cterm=bold ctermfg=4
hi Title        ctermfg=3 cterm=bold
hi Statement    cterm=none ctermfg=3

hi Constant   cterm=none ctermfg=1
hi Identifier cterm=none ctermfg=6
hi Type       cterm=none ctermfg=2
hi Special    cterm=none ctermfg=5

if &t_Co > 16 && $TERM != "screen"
    "hi Operator ctermfg=45
    hi Identifier cterm=none ctermfg=23
    hi Comment    cterm=none ctermfg=81
    hi LineNr     ctermfg=73 ctermbg=80
    hi Folded     cterm=none ctermfg=73 ctermbg=80
    hi Title      ctermfg=34
endif

hi clear PreProc
hi link PreProc Special

hi SpecialKey ctermfg=white ctermbg=5 cterm=none

"hi link luaFunction Keyword
"hi link luaFunction Keyword
