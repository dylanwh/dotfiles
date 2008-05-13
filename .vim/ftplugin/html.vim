"-- do not hightlight <u>, etc.
let html_no_rendering=1

source ~/.vim/closetag.vim

inoremap <buffer> <C-P> <p> </p><ESC>4hi
nmap     <buffer> <C-P> a<p> </p><ESC>4hi

inoremap <buffer> <silent> <C-L> <C-O>:call HTMLMakeLink()<CR>
nmap     <buffer> <silent> <C-L> :call HTMLMakeLink()<CR>

command! Img call HTMLMakeImg()
command! A call HTMLMakeLnk()

function! HTMLMakeLink()
	let href=input("href: ")
	let text=input("text: ")
	let xreg=@x
	let @x='<a href="' . href . '">' . text . "</a>"
	norm "xgp
	let @x=xreg

endfunction

function! HTMLMakeImg()
	let src=input("src: ")
	let alt=input("alt: ")
	let xreg=@x
	let @x='<img src="' . src . '" alt="' . alt . '"/>'
	norm "xgp
	let @x=xreg
endfunction
