" Vim color file
" Maintainer:   Dylan Hardison <dylanwh@gmail.com>
" Last Change:	16-Apr-2008.

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

let colors_name = "dylan"
set bg=dark

hi StatusLine   cterm=none ctermbg=4
hi StatusLineNC cterm=none ctermbg=4 ctermfg=darkgray
hi TabLine      ctermbg=4 ctermfg=7 cterm=none
hi TabLineFill  ctermbg=4 cterm=none
hi VertSplit    term=none cterm=none ctermbg=4
hi Folded       cterm=bold ctermbg=none ctermfg=5
hi LineNr       ctermfg=4
hi Directory    cterm=bold ctermfg=4

hi Normal     cterm=none ctermfg=7

hi Comment cterm=bold ctermfg=4

hi Statement  cterm=none ctermfg=3
"hi Statement  cterm=none ctermfg=34

hi Constant   cterm=none ctermfg=1
"hi Constant   cterm=none ctermfg=12

hi Identifier cterm=none ctermfg=6
"hi Identifier cterm=none ctermfg=22

hi Type       cterm=none ctermfg=2
hi Special    cterm=none ctermfg=5

hi clear PreProc
hi link PreProc Special 

" Remove all existing highlighting and set the defaults.
"if &term == 'rxvt-unicode'
"  set term=rxvt
"endif

" vim: sw=2
