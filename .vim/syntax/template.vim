" Vim syntax file
" Language:     Template Toolkit
" Maintainer:   Clifton Wood <cliff@slashdot.org>
" Last Change:  2002 May 30
" Location:     <none yet>
"
" Thanks to Andy Wardley and the entire TT crew. 
"
" I felt it was high time to contribute something back to the Template 
" community.
"
" HEAVILY borrows from the rules from perl.vim. 
" 'Standing on the shoulders of Giants', never applied more than it does here. 
" Credit for this syntax file should also go to the authors of perl.vim, and of
" course, those involved with VIM itself.
"
" Please download most recent version first before mailing
" any comments.
"

" Config:
"   unlet template_no_html  include html stuff.
"   unlet template_eval_perl    include perl stuff.
"   unlet template_interpolate  interpolate $vars.


let template_with_html = 1

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" The following is copied from the mason syntax file
" First pull in the HTML syntax.

if !exists("main_syntax")
	let main_syntax = 'template'
endif

if !exists('template_no_html')
	if version < 600
		so <sfile>:p:h/html.vim
	else
		runtime! syntax/html.vim
		unlet b:current_syntax
	endif
endif

if !exists('template_no_html')
	syn cluster htmlPreproc add=@TemplateVarReplace
endif

" Now pull in the Perl syntax.


if exists('template_eval_perl')
	if version < 600
		syn include @perlTop <sfile>:p:h/perl.vim
	else
		syn include @perlTop syntax/perl.vim
	endif
endif


" All Template syntax items will appear inside of the default TAG syntax  item,
" defined below.

" $variables and ${variables}.
syn match TemplateVar	contained  "\a\i*\(\.\(\(\$\=\i*\)|\${.*}\)\+\)\=\>"  contains=TemplateVarOp,TemplateNum

" Variables in double quoted strings. These may eventually need to be
" combined. Should handle the following:
"
" 	$template_var
" 	$template.var
" 	$template.${var}
" 	$template.${template.var}
"
" For simplicity's sake, if you start a template var with a capital  letter,
" you risk colliding with a directive, therefore colorization will not  attempt
" to color those entries. You're on your own there. If someone has the  ability
" to remove this limitation, please do.
syn match TemplateStrV	contained "\${\a\i*\(\.\(\\I\i*|\d\+\)\)\=}"  contains=TemplateVarOp,TemplateNum,TemplateSeparator
syn match TemplateStrV	contained  "\$\a\i*\(\.\(\I\i*\|${\I\i*}\|\d\+\)\)\="  contains=TemplateVarOp,TemplateNum,TemplateSeparator

" Standard Template variable methods
syn keyword TemplateVarOp contained defined replace reverse unshift
syn keyword TemplateVarOp contained exists import length repeat search  values
syn keyword TemplateVarOp contained first match nsort shift split
syn keyword TemplateVarOp contained each grep hash item join keys last  list
syn keyword TemplateVarOp contained push size sort max pop

" Keywords
syn keyword TemplateAccess	contained GET CALL SET DEFAULT META
syn keyword TemplateProcess	contained INSERT INCLUDE PROCESS WRAPPER  BLOCK
syn keyword TemplateProcess	contained CLEAR TAGS
syn keyword TemplateConditional	contained IF UNLESS ELSIF ELSE SWITCH  CASE
syn keyword TemplateLoop	contained FOR FOREACH WHILE NEXT LAST BREAK IN
syn keyword TemplateFilter	contained FILTER USE MACRO PERL RAWPERL
syn keyword TemplateException	contained TRY THROW CATCH FINAL
syn keyword TemplateControl	contained STOP RETURN END

" Numbers and Floats. 
" (These rules borrowed from perl.vim for my own nefarious purposes).
syn match TemplateNum	contained  "[-+]\=\(\<\d[[:digit:]_]*L\=\>\|-[xX]\x[[:digit:]_]*\>\)"
syn match TemplateFloat	contained  "[-+]\=\<\d[[:digit:]_]*[eE][\-+]\=\d+"
syn match TemplateFloat contained  "[-+]\=\<\d[[:digit:]_]*\.[[:digit:]_]*\([eE][\-+]\=\d\+\)\="
syn match TemplateFloat contained  "[-+]\=\<\.[[:digit:]_]\+\([eE][\-+]\=\d\+\)\="

" Operators

" The basics were kinda tricky to get down right. It would be nicer if  the
" patterns could be more specific. I haven't been able to play with  these 
" enough to get them working to my taste, yet. "Good enough for gov't  work", 
" though. =D
syn match TemplateOperator	contained "\s*[*!+-/=|<>]\s*"

syn match TemplateOperator	contained "=="			" ==
syn match TemplateOperator	contained "<="			" <=
syn match TemplateOperator	contained ">="			" >=
syn match TemplateOperator	contained "||"			" ||
syn match TemplateOperator	contained "&&"			" &&
syn match TemplateOperator	contained "!="			" !=
syn match TemplateOperator	contained "\.\."		
syn match TemplatePipeOperator	contained "|"			" | (filter)
syn keyword TemplateOperator	contained MOD DIV AND OR NOT
syn keyword TemplateOperator	contained mod div and or not

" String interpolation.
syn match TemplateString	contained "\\\(\d\+\|[xX]\x\+\|c\u\|.\)"
syn match TemplateStringU	contained "\\['\\]"

" Command delimeter. (Sometimes it helps if you can visibly see the  semi's)
syn match TemplateSeparator	contained "\>\s*;"

syn cluster TemplateDoubleQuote contains=TemplateString,TemplateStrV
syn cluster TemplateSingleQuote contains=TemplateStringU

" Like perl, the '=>' operator forces a barewword to the left to be
" interpreted as a string. This should probably be under its own group 
" so that it can be contained in an eventual '{}' assignment region.
syn match TemplateHashString	contained "\<\I\i*\s*=>"me=e-2

syn region TemplateSQString	contained  matchgroup=TemplateStringStartEnd start="'" end="'"  contains=@TemplateSingleQuote
syn region TemplateDQString	contained  matchgroup=TemplateStringStartEnd start=+"+ end=+"+  contains=@TemplateDoubleQuote

" {} Blocks.
syn region TemplateCurlyBlock contained matchgroup=TemplateOperator  start='{' end='}' contains=TemplateHashString,TemplateSQString,TemplateDQString,TemplateNum,TemplateFloat,TemplateVar

" Single line template comment.
syn match TemplateSingleComment contained "#.*"

" ATTEMPT to highlight filternames.
syn match TemplateFilterName	contained "\l\i*"
syn region TemplateFilterExpr	contained start="|\s*\l\i*"  end=";"he=e-1 contains=TemplatePipeOperator,TemplateFilterName
syn region TemplateFilterExpr	contained start="|\s*\l\i*"  end="%]"he=e-2 contains=TemplatePipeOperator,TemplateFilterName
syn region TemplateFilterExpr	contained start="|\s*\l\i*"  end="\(-|+\)%]"he=e-3  contains=TemplatePipeOperator,TemplateFilterName

" What is legal inside of a tag?
syn cluster TemplateDirectiveContents  contains=TemplateAccess,TemplateProcess,TemplateConditional,TemplateLoop,TemplateFilter,TemplateFilterExpr,TemplateException,TemplateControl,TemplateOperator,TemplatePipeOperator,TemplateSingleComment,TemplateVar,TemplateNum,TemplateFloat,TemplateDQString,TemplateSQString,TemplateSeparator,TemplateCurlyBlock


"syn cluster TemplateDirectiveContents  contains=TemplateAccess,TemplateProcess,TemplateConditional,TemplateLoo p,TemplateFilter,TemplateFilterExpr,TemplateException,TemplateControl,Tem plateOperator,TemplatePipeOperator,TemplateSingleComment,TemplateNum,Temp lateFloat,TemplateDQString,TemplateSQString,TemplateSeparator

" '[%(-+)?' '(-+)?%]' -- This is the syntax group that matches any given
" directives and its contents should be highlighted accordingly.
"
" Bless you, "matchgroup" -- as it highlights the "start" and "end"  patters
" appropriately. If it weren't for matchgroup, the POST/PRECHOMP chars  would
" be highlighted as operators.
syn region TemplateDirective matchgroup=TemplateTags  start="\[%[-+]\=" end="[-+]\=%\]" keepend  contains=@TemplateDirectiveContents

" Multiline template comments are handled by their own region.
" (No need for "matchgroup" here since this region doesn't contain
" TemplateOperator)
syn region TemplateComment start="\[%#" end="[-+]\=%\]" keepend

if exists('template_interpolate')
	syn match TemplateVarReplace "\$[A-Za-z0-9_\.]\+" contains=TemplateVarOp,TemplateOperator,TemplateNum
endif

" Now we assign highlight groups.
if version >= 508 || !exists("did_template_syn_inits")
  if version < 508
    let did_template_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink TemplateAccess		Include
  HiLink TemplateProcess	PreProc
  HiLink TemplateConditional	Conditional
  HiLink TemplateLoop		Repeat
  HiLink TemplateFilter		Statement
  HiLink TemplateException	Special
  HiLink TemplateControl	Repeat

  HiLink TemplateVarOp		 Type
  HiLink TemplateVar 		 Identifier
  HiLink TemplateVarReplace Identifier
  
  HiLink TemplateNum		Number
  HiLink TemplateFloat		Float

  HiLink TemplateOperator	Operator
  HiLink TemplatePipeOperator	Operator

  HiLink TemplateFilterName	Statement

  HiLink TemplateHashString	String
  HiLink TemplateString		String
  HiLink TemplateSQString	Constant
  HiLink TemplateDQString	String
  HiLink TemplateStringStartEnd String
  HiLink TemplateStrV		Identifier

  HiLink TemplateDirective	Delimiter
  HiLink TemplateTags		Delimiter

  HiLink TemplateSeparator	SpecialChar

  HiLink TemplateComment	Comment
  HiLink TemplateSingleComment	Comment

  delcommand HiLink
endif

" Standard variable methods, don't get the preceeding dot-separator.

" Identify the current syntax.
let b:current_syntax = "template"

" vim: ts=3
