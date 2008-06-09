" Vim filetype plugin file
" Language:	Perl
" Maintainer:	Dylan William Hardison <dylan@hardison.net>
" Last Change:  2008-06-09.


let b:did_ftplugin = 1

"-- Pretty folding.
setlocal foldtext=PerlFoldTextNoLines()
setlocal formatoptions+=crq
setlocal comments=:#
setlocal commentstring=#%s
setlocal include=\\<\\(use\\\|require\\)\\>
setlocal includeexpr=substitute(substitute(v:fname,'::','/','g'),'$','.pm','')
setlocal define=[^A-Za-z_]
setlocal keywordprg=perldoc\ -f
setlocal textwidth=100
set isfname+=:

if $HOST == 'mani'
	setlocal expandtab
end

"-- Do not fold package-level things.
if expand("%:e") == 'pm'
	setlocal foldlevel=1
endif

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

" Undo the stuff we changed.
let b:undo_ftplugin = "setlocal fo< com< cms< inc< inex< def< isf<"

if has("perl")
perl <<PERL
	my $path = join(',', @INC);
	$path =~ s/\.\///g;
	VIM::DoCommand("let perlpath='$path'");
	$path =~ s/ /\\ /g;
	VIM::DoCommand("setlocal path+=$path");
PERL
endif

"-- toggle displaying numbers at the end.
map <silent> z; :silent call TogglePerlFold()<CR>
map <silent> z/ :silent setlocal foldtext<<CR>

command! -range=% PerlTidy <line1>,<line2>!perltidy -pbp -l=100

function! TogglePerlFold()
	if &foldtext == "PerlFoldTextNoLines()"
		let &l:foldtext = "PerlFoldText()"
	else
		let &l:foldtext = "PerlFoldTextNoLines()"
	endif
endfunction

function! s:Repeat(nr, char)
	let nr = a:nr
	let s = ""
	while nr > 0
		let nr = nr - 1
		let s = s . a:char
	endwhile
	return s
endfunction

function! PerlFoldText()
	let txt = substitute(foldtext(), "{", "", "g")
	let result = substitute(txt, " *\\(\\d\\+ lines\\): \\(.\\+\\)\\s*$", " \\2#=#\\1", "g")
	let spaces = winwidth(0) - strlen(result) + 1
	return substitute(result, "\#=#", s:Repeat(spaces, " "), "g")
endfunction

function! PerlFoldTextNoLines()
	let txt = PerlFoldText()
	return substitute(txt, " \\+\\d\\+ lines$", "", "")
endfunction
