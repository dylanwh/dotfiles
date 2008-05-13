" Vim syntax file
" Language:	embperl
" Maintainer:	Lukas Zapletal <Lukas.Zapletal@seznam.cz>
" Version: 1.2
" URL: http://vim.sourceforge.net/scripts/search_results.php?keywords=Embperl
" Last change:	2001 May 1
"
" Special thanks: Steve Willner, author of 5.x version

" USE:
" 
" 	let perl_fold=1		" note enabling this will slow down synchronizing
"		augroup filetypedetect
"		autocmd! BufNewFile,BufRead *.epl,*.phtml setf embperl
"		augroup END
"
"		" if you want yellow code like in Interdev:
"		autocmd BufNewFile,BufRead *.epl,*.phtml colorscheme embperl_yellow
"
"	CHANGELOG:
"	v1.0 - initial release
"	v1.1	- increased min. lines in syncing
"				- [$ $] now completely fixed
"				- minor bugfixes and documentation update
"	v1.2	- minor bugfixes
"
" TODO:
" - folding for embperl`s '[+ [- [$ ...'

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'embperl'
endif

if version < 600
	source <sfile>:p:h/html.vim
	syn include @EmbperlPerl <sfile>:p:h/perl.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
  syn include @EmbperlPerl syntax/perl.vim
endif

syn cluster htmlPreproc add=EmbperlInsideHtml

" these are the embperl regions, which simply contain perl expressions (except for the special [$ mode)
"syn keyword embperlMeta contained if elsif else while foreach endif endwhile endforeach vars hidden endsub
syn keyword embperlMeta contained if elsif else endif while endwhile do until foreach endforeach hidden var sub endsub
syn region EmbperlInsideHtml matchgroup=Tags start="\[+"rs=s end="+\]"re=e contains=@EmbperlPerl, perlPackageStatement
syn region EmbperlInsideHtml matchgroup=Tags start=+\[-+rs=s end=+-\]+re=e contains=@EmbperlPerl, perlPackageDecl
syn region EmbperlInsideHtml matchgroup=Tags start=+\[!+rs=s end=+!\]+re=e contains=@EmbperlPerl, perlPackageDecl
syn region EmbperlInsideHtml matchgroup=Tags start=+\[\$+rs=s end=+\$\]+re=e contains=embperlMeta, perlConditional, perlOperator

hi link embperlMeta 		perlStatement
hi link EmbperlComment  Comment
syn region EmbperlComment    matchgroup=Tags start="\[#"rs=s end="#\]"re=e

" syncing
syntax sync minlines=40
syntax sync maxlines=100

let b:current_syntax = "embperl"

if main_syntax == 'embperl'
  unlet main_syntax
endif

" vim: set tabstop=2 shiftwidth=2 noexpandtab:
