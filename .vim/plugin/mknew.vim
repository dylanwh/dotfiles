autocmd BufNewFile  * exec "silent 0read! mknew -t" . shellescape(&ft ? &ft : "auto") . " -f" . shellescape(expand("%"))
