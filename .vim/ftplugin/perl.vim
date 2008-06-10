"-- Pretty folding.
setlocal foldtext=PerlFoldTextNoLines()

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

setlocal textwidth=100

if $HOST == 'mani'
	setlocal expandtab
end

"-- Do not fold package-level things.
if expand("%:e") == 'pm'
	setlocal foldlevel=1
endif

"-- Do not highlight 'new'!
hi link perlStatementNew NONE

"-- toggle displaying numbers at the end.
map <silent> z; :silent call TogglePerlFold()<CR>
map <silent> z/ :silent setlocal foldtext<<CR>

command! -range=% PerlTidy <line1>,<line2>!perltidy -pbp -l=100

function! TogglePerlFold()
	if &l:foldtext == "PerlFoldTextNoLines()"
		setlocal foldtext=PerlFoldText()
	else
		setlocal foldtext=PerlFoldTextNoLines()
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
