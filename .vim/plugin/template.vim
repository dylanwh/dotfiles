autocmd BufNewFile             * call Template()
function! Template()
	let file = "template/" . &filetype . ".vim"
	exec "runtime! " file
	norm i
endfunction
