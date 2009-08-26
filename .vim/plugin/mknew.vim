autocmd BufNewFile  * exec "silent 0read! mknew -m -f" . shellescape(expand("%"))
