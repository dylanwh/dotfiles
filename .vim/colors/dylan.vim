" Vim color file
" Maintainer:   Dylan Hardison <dylanwh@gmail.com>
" Last Change:	16-Sep-2007.





" Remove all existing highlighting and set the defaults.
if &term == 'rxvt-unicode'
  set term=rxvt
endif

set bg=light

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif
hi clear Normal
highlight clear
highlight Normal guibg=Black guifg=White
highlight StatusLine cterm=bold ctermbg=darkblue
highlight StatusLineNC gui=none guibg=darkblue
highlight StatusLine   gui=bold guibg=darkblue
highlight StatusLineNC cterm=none ctermbg=darkblue
highlight VertSplit term=none cterm=none gui=none ctermbg=darkblue guibg=darkblue

highlight Pmenu ctermfg=white ctermbg=blue
highlight PmenuSel ctermfg=blue ctermbg=cyan
highlight PmenuSbar ctermbg=blue ctermfg=red
highlight PmenuThumb ctermbg=blue ctermfg=white

highlight Folded cterm=none ctermbg=none ctermfg=magenta
highlight LineNr ctermfg=darkblue
highlight FoldColumn guibg=darkgrey guifg=white
highlight Comment cterm=bold ctermfg=blue
highlight Directory cterm=bold ctermfg=blue
highlight TabLine ctermbg=darkblue ctermfg=white cterm=none
highlight TabLineSel ctermbg=darkblue ctermfg=green cterm=none
highlight TabLineFill ctermbg=darkblue cterm=none

"highlight NonText ctermfg=1

let colors_name = "dylan"

" vim: sw=2
