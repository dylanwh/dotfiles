function get_mutt_var(file, name)
    local home = os.getenv("HOME")
    local cmd = string.format("mutt -F '%s' -Q '%s'", home .. "/.mutt/" .. file, name)
    local str = io.popen(cmd):read()
    local m = str:match(name .. '="(.+)"')
    if m then
        return m
    else
        error("can't get mutt var")
    end
end

local home = IMAP {
    server = 'imap.gmail.com',
    username = 'dylan@hardison.net',
    password = get_mutt_var("passwd.home", "imap_pass"),
    ssl = 'ssl3',
}

local work = IMAP {
    server = 'imap.gmail.com',
    username = 'dylan.hardison@iinteractive.com',
    password = get_mutt_var("passwd.work", "imap_pass"),
    ssl = 'ssl3',
}

function main()
    -- trash rules
    trash(home, "daily",        is_older(0))
    trash(home, "facebook",     is_older(1))
    trash(home, "sms",          is_older(7))
    trash(home, "security",     is_older(7))
    trash(home, "suckless",     is_older(7))
    trash(home, "poe",          is_older(7))
    trash(home, "templates",    is_older(7))
    trash(home, "caml",         is_older(7))
    trash(home, "netflix",      is_older(5))
    trash(home, "haskell-cafe", is_older(2))

    -- archive rules
    move(home, "slug",    "slug/archive",    is_expired(30))
    move(home, "billing", "billing/archive", is_expired(30))

    copy(home, '[Gmail]/All Mail', 'ignored', is_ignored(7))

    trash(work, "issue",  is_older(7))
    trash(work, "linode", is_older(7))
    trash(work, "daily",  is_older(0))
end

function copy(imap, from_name, to_name, f)
    local from = imap[from_name]
    local to   = imap[to_name]
    from:copy_messages(to, f(from))
end

function move(imap, from_name, to_name, f)
    local from = imap[from_name]
    local to   = imap[to_name]
    from:move_messages(to, f(from))
end

function trash(imap, name, f)
    move(imap, name, '[Gmail]/Trash', f)
end

function trash_all(imap, f)
    trash(imap, '[Gmail]/All Mail', f)
end

function is_ignored(age)
    return function (_) return _:is_older(age) * _:is_unseen() end
end

function is_older(age) 
    return function (_) return _:is_older(age) * _:is_unflagged() end 
end

function is_expired(age)
    return function (_) return _:is_older(age) * _:is_seen() * _:is_unflagged() end
end


main()
