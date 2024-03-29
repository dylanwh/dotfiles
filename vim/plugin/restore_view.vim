"          FILE: restore_view.vim
"      Language: vim script
"    Maintainer: Dylan William Hardison <dylan@hardison.net>
"       Created: 2012-02-27 16:12:12
" Last Modified: 2013 May 20 13:52:48
"   Description: 
"       This is a simple script to autosave cursor position and fold
"       information using vim's mkview.  Although you can easily do this job by
"       just add serveral line to {.,_}vimrc, write a script plugin can make it
"       more clean and nice.  We assume you use a new enough Vim to enjoy
"       these feature. Hope you love it:)
"
"       Views will be saved when you save/write a file or EXIT VIM.
"
" Suggested Setting:
"       Please put them in you vimrc file.
"           set viewoptions=cursor,folds,slash,unix
"       
"       Set it in a plugin file looks dirty to me.  So you'd better do it your
"       self.  This only keywords not in viewoptions is "options". I believe it
"       does not belong to a view.  If you think you need it, feel free to
"       put it in.  If you do not want views of some files to be saved, please
"       set g:loaded_restore_view. The longer time you use, the bigger view
"       folder you will have.  So if you use UNIX environment, you may need to
"       use cron to do some clean job.
"
"       Most of code is from wiki.

if exists("g:loaded_restore_view")
    finish
endif

let g:loaded_restore_view = 1

if !exists("g:restore_view_ignore")
    let g:restore_view_ignore = [ "^/tmp", ".git/COMMIT_EDITMSG$" ]
endif

function! RestoreViewAllowed()
    if empty(expand("%")) | return 0 | endif
    if has('quickfix') && &buftype =~ 'nofile' | return 0 | endif
    if &modifiable == 0 | return 0 | endif

    let path = expand('%:p')
    for regexp in g:restore_view_ignore
        if path =~# regexp
            return 0
        endif
    endfor

    return 1
endfunction

augroup RestoreView
    autocmd!
    autocmd BufWritePost,WinLeave,BufWinLeave ?* if RestoreViewAllowed() == 1 | mkview | endif
    autocmd BufWinEnter ?* if RestoreViewAllowed() == 1 | silent! loadview | endif
augroup END
