if have exa
    alias ls "exa -Fh --group-directories-first"
else if command ls --group-directories-first 2>&1 | grep -q illegal
    gnu_alias ls "-Fh --color=auto --group-directories-first"
    or alias ls "ls -GFh"
else
    alias ls "ls -Fh --color=auto --group-directories-first"
end
