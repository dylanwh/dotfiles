autocmd BufNewFile  * exec "silent 0read! mknew -f" . shellescape(expand("%"))
