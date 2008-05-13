" Vim filetype plugin
" Language:     CAOS
" Maintainer:   Francis Irving (francis.irving@creaturelabs.com) 
"
" Modified by Meikel Brandmeyer <Brandels_Mikesh@web.de> in order
" to fit into the ftplugin-scheme of vim.
" Split into:
"  - ftplugin/caos.vim
"  - syntax/caos[.auto].vim
"  - indent/caos.vim
"
" Listen carefully! I'll say that only once.
if exists("b:did_ftp_plugin")
    finish
endif
let b:did_ftplugin = 1

setlocal iskeyword=@,48-57,_,192-255,:,<,=,>,#,+

" Set some defaults:
if !exists("CaosUseGui")
    let CaosUseGui = 0
endif

if !exists("CaosUseWinStd")
    let CaosUseWinStd = 1
endif

" Note: This uses nc which is netcat - you'll need to install it
"
" If $HOME/.dockingstation/port is not empty we will use the value there.
" Port lacks a trailing newline, so may be cat alone does not work, because
" the output buffer is not flushed.
let s:port = system("echo -n `cat $HOME/.dockingstation/port`")
if s:port == ''
    let s:port = 20001
endif

" Normally, we save the code for injection in a temporary file and inject
" it with this function via netcat.
if !exists("*s:DoInjectTempfile")
    function s:DoInjectTempfile(tempfile)
	let a = system('nc localhost '. s:port .' <' . a:tempfile)
"	execute ":bdelete! " . a:tempfile
	
	if v:shell_error
	    return "Failed to connect to game"
	endif

	return a
    endfunction
endif

" Injects the current buffer.
if !exists("*s:Inject")
    function s:Inject()
	let tempfile = tempname()
	execute ":w " . tempfile
	
	let a = system('echo -e "\nrscr\n" >>' . tempfile)
	
	return s:DoInjectTempfile(tempfile)
    endfunction
endif

" Inject the remove script of current buffer.
if !exists("*s:UnInject")
    function s:UnInject()
	let tempfile = tempname()
	execute "\:1/rscr/+1,$w " . tempfile
	
	let a = system('echo -e "\nrscr\n" >>' . tempfile)

	return s:DoInjectTempfile(tempfile)
    endfunction
endif

if !exists("*s:ChooseInject")
    function s:ChooseInject()
	let tempfile = tempname()
	
	if has("gui_running") && g:CaosUseGui == 1
	    let command = inputdialog("CAOS command: ")
	else
	    let command = input("CAOS command: ")
	endif
	
	echo "\n"

	" TODO: Work out how to do this better in vim
	" (make a new buffer, and append() to that?)
	let command = substitute(command, "\\", "\\\\\\\\", "g")
	let command = substitute(command, "\"", "\\\\\\\"", "g")
	let retval = system('echo ' . command . ' >>' . tempfile)
	let retval = system('echo -e "\nrscr\n" >>' . tempfile)

	echo s:DoInjectTempfile(tempfile)
    endfunction
endif

" Looks at the word under the cursor and gets help on this word from DS
if !exists("*s:MANN")
    function s:MANN(word)
	let cur_win = winnr()
	
	" If word is empty, then we ask the user to enter a string.
	if a:word == ''
	    if has("gui_running") && g:CaosUseGui == 1
		let lookup = inputdialog("Look up help for: ")
	    else
		let lookup = input("Look up help for: ")
	    endif
	else
	    let lookup = a:word
	endif

	let tempfile = tempname()
	let a = system('echo -e "mann \"' . lookup . '\"\nrscr\n" >>' . tempfile)
	
	echo s:DoInjectTempfile(tempfile)
    endfunction
endif


if !exists("*s:APRO")
    function s:APRO()
	let tempfile = tempname()
	
	if (has("gui_running") && g:CaosUseGui == 1)
	    let search_string = inputdialog("Search string:")
	else
	    let search_string = input("Search string: ")
	endif
	
	let a = system('echo -e "apro \"'. search_string .'\"\nrscr\n" >> '. tempfile)

	echo s:DoInjectTempfile(tempfile)
    endfunction
endif

if !hasmapto('<Plug>CaosInject')
    if CaosUseWinStd == 1
	nmap <buffer> <unique> <F11> <Plug>CaosInject
    else
	nmap <buffer> <unique> <LocalLeader>i <Plug>CaosInject
    endif
endif
nnoremap <buffer> <unique> <script> <Plug>CaosInject <SID>Inject
nnoremap <SID>Inject :call <SID>Inject()<CR>

if !hasmapto('<Plug>CaosUnInject')
    if CaosUseWinStd == 1
	nmap <buffer> <unique> <F12> <Plug>CaosUnInject
    else
	nmap <buffer> <unique> <LocalLeader>u <Plug>CaosUnInject
    endif
endif
nnoremap <buffer> <script> <Plug>CaosUnInject <SID>UnInject
nnoremap <SID>UnInject :call <SID>Uninject()<CR>

if !hasmapto('<Plug>CaosChooseInject')
    nmap <buffer> <unique> <LocalLeader>c <Plug>CaosChooseInject
endif
nnoremap <buffer> <script> <Plug>CaosChooseInject <SID>ChooseInject
nnoremap <SID>ChooseInject :call <SID>ChooseInject()<CR>

if !hasmapto('<Plug>CaosMANN')
    nmap <buffer> <unique> <LocalLeader>m <Plug>CaosMANN
endif
nnoremap <buffer> <script> <Plug>CaosMANN <SID>MANN
nnoremap <SID>MANN :call <SID>MANN(expand("<cword>"))<CR>

if !hasmapto('<Plug>CaosAPRO')
    nmap <buffer> <unique> <LocalLeader>a <Plug>CaosAPRO
endif
nnoremap <buffer> <script> <Plug>CaosAPRO <SID>APRO
nnoremap <SID>APRO :call <SID>APRO()<CR>


" vim: ts=8 sts=4 sw=4
