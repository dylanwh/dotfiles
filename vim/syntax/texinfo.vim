" Vim syntax file
" Language:	Texinfo
" Maintainer: Dylan William Hardison <dhardison@cpan.org>
" Last Change:	26-Mar-2005.
" Remark:	Better than the default texinfo.vim :)
" Remark:   Only works in vim 6.x and higher.

" For version 6.x: Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" texinfo is case sensitive. I think.
syn case match


" Mark the TeX \input command and filename as a special thing.
syn match texiSpecial "\\input\s\+texinfo"
" Mark @bye as special too.
syn region texiSpecial start="@bye\n\?"    end="^\n\n\n$"


syn region texiParam matchgroup=texiFunction start="@\w\+{" end="}" skip="@\w\+{.*" 
			\ contains=texiParam,texiFunction,texiChar,texiMacroVar

syn region texiParam matchgroup=texiStatement start="^@\w\+ " 
			\ skip="\\$" end="$" contains=texiFunction,texiParam,texiChar oneline
syn match texiStatement "@end \w\+"
syn match texiStatement "^@\w\+$"


" @ignore is colored like any other @-statement, but the contents are comment color.
syn region texiComment matchgroup=texiStatement start="^@ignore\s*$" end="^@end ignore\s*$"
" Comments.
syn match texiComment "@c$"
syn match texiComment "@c .*$"
syn match texiComment "@comment .*"

syn match texiSet    /^@set / nextgroup=texiSetVar skipwhite
syn match texiSetVar /[A-Za-z0-9]\+/ contained nextgroup=texiSetVal skipwhite
syn match texiSetVal /.*$/ contained

syn region texiParam  matchgroup=texiStatement start="^@macro" end="^@end macro" 
			\ contains=texiMacroVar,texiFunction,texiChar,texiStatement,texiParam
syn match texiMacroVar /\\\w\+\\/



" TODO: add support for named menu items.
syn region texiMenu matchgroup=texiStatement start="^@menu\s*$" end="^@end menu\s*$" 
			\ contains=texiMenuItem,texiMenuMarker
syn region texiMenuItem matchgroup=texiMenuMarker start=/^* / end="::" contained oneline


" Borrowing this from the older texinfo.vim
syn match texiChar /@["'=^`~][aeinouyAEINOUY]/
syn match texiChar "@\(!\|?\|@\|\s\)"
syn match texiChar "@{"
syn match texiChar "@}"
" accents
syn match texiChar "@=."
syn match texiChar "@dotaccent{.}"
syn match texiChar "@H{.}"
syn match texiChar "@,{[cC]}"
syn match texiChar "@AA{}"
syn match texiChar "@aa{}"
syn match texiChar "@L{}"
syn match texiChar "@l{}"
syn match texiChar "@O{}"
syn match texiChar "@o{}"
syn match texiChar "@ringaccent{.}"
syn match texiChar "@tieaccent{..}"
syn match texiChar "@u{.}"
syn match texiChar "@ubaraccent{.}"
syn match texiChar "@udotaccent{.}"
syn match texiChar "@v{.}"
"ligatures
syn match texiChar "@AE{}"
syn match texiChar "@ae{}"
syn match texiChar "@copyright{}"
syn match texiChar "@bullet" contained "for tables and lists
syn match texiChar "@bullet{}"
syn match texiChar "@dotless{i}"
syn match texiChar "@dotless{j}"
syn match texiChar "@dots{}"
syn match texiChar "@enddots{}"
syn match texiChar "@equiv" contained "for tables and lists
syn match texiChar "@equiv{}"
syn match texiChar "@error{}"
syn match texiChar "@exclamdown{}"
syn match texiChar "@expansion{}"
syn match texiChar "@minus" contained "for tables and lists
syn match texiChar "@minus{}"
syn match texiChar "@OE{}"
syn match texiChar "@oe{}"
syn match texiChar "@point" contained "for tables and lists
syn match texiChar "@point{}"
syn match texiChar "@pounds{}"
syn match texiChar "@print{}"
syn match texiChar "@questiondown{}"
syn match texiChar "@result" contained "for tables and lists
syn match texiChar "@result{}"
syn match texiChar "@ss{}"
syn match texiChar "@TeX{}"



" Define the default highlighting.

hi def link texiComment Comment
hi def link texiSpecial Special
hi def link texiStatement Statement
hi def link texiFunction Function
hi def link texiParam String
hi def link texiChar Special
hi def link texiSet Statement
hi def link texiSetVar Identifier
hi def link texiMacroVar Identifier
hi def link texiSetVal texiParam

hi def link texiMenuItem Underlined
hi def link texiMenuMarker Type
hi def link texiMenu texiComment




let b:current_syntax = "texinfo"
