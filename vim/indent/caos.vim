" Vim indent file
" Language:     CAOS
" Maintainer:   Francis Irving (francis.irving@creaturelabs.com) 
"
" Modified by Meikel Brandmeyer <Brandels_Mikesh@web.de> in order
" to fit into the ftplugin-scheme of vim.
" Split into:
"  - indent/caos.vim
"  - ftplugin/caos.vim
"  - syntax/caos[.auto].vim
"
" Listen carefully! I'll say that only once.
if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

" Set indenting-expressions
setlocal indentexpr=CaosIndentLine(v:lnum)
setlocal indentkeys+=0=endm,0=repe,0=endi,0=untl,0=retn,0=ever,0=else,0=elif,!,o,O

"
" Configuration variables:
"	- caos_ind_incr:	increase indetation eg. after an doif
"				by this amount (default = shiftwidth)
if !exists("CaosIndent")
    let CaosIndent = &sw
endif

"
" Internal configuration:
" (Please do *NOT* edit or things will break!)
let s:caos_incr_words = '\(doif\|else\|elif\|iscr\|rscr\|scrp\|reps\|loop\|subr\)'
let s:caos_decr_words = '\(else\|elif\|endi\|untl\|endm\|ever\|repe\|retn\)'

"
" function CaosIndentLine(start_lnum)
"	    - start_lnum: linenumber of line to reindent
"
" Computes the indent of the given line.
" Returns: amount of indentation
"
if !exists("CaosIndentLine")
    function CaosIndentLine(start_lnum)
	let amount = 0
	let offset = 0
	let lnum = a:start_lnum

	let theline = getline(lnum)

	" Do not indent empty lines
	if theline == $
	    return 0
	endif

	" Decrease offset, if closing keyword was found in current line.
	if match(theline, '^[\t ]*'.s:caos_decr_words) >= 0
	    let offset = offset - g:CaosIndent
	endif

	" We skip empty lines.
	let lnum = prevnonblank(lnum - 1)
    
	" Now go back until we find something we can use.
	if (lnum > 0)
	    let theline = getline(lnum)

	    if match(theline, '^[\t ]*'.s:caos_incr_words) >= 0
		" We found an opening keyword, so increase indent.
		let amount = indent(lnum) + g:CaosIndent + offset
	    else
		" Nothing of interest. Take the indentation of
		" this line, eventually with an offset.
		let amount = indent(lnum) + offset
	    endif
	endif

	return amount
    endfunction
endif
