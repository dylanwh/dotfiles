if have eza
    alias ls "eza -Fh --group-directories-first"
else if command ls --group-directories-first 2>&1 | egrep -q 'illegal|unrecognized'
    gnu_alias ls "-Fh --color=auto --group-directories-first"
    or alias ls "ls -GFh"
else
    alias ls "ls -Fh --color=auto --group-directories-first"
end
