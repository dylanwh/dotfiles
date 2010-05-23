-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

function main()
    -- my normal @hardison.net account
    local home = IMAP {
        server = 'imap.gmail.com',
        username = 'dylan@hardison.net',
        password = get_mutt_var("passwd.home", "imap_pass"),
        ssl = 'ssl3',
    }

    -- my work email account
    local work = IMAP {
        server = 'imap.gmail.com',
        username = 'dylan.hardison@iinteractive.com',
        password = get_mutt_var("passwd.work", "imap_pass"),
        ssl = 'ssl3',
    }

    -- trash rules
    trash(home, "daily",        is_older(0))
    trash(home, "message",      is_older(1))
    trash(home, "security",     is_older(7))
    trash(home, "suckless",     is_older(7))
    trash(home, "poe",          is_older(7))
    trash(home, "templates",    is_older(7))
    trash(home, "caml",         is_older(7))
    trash(home, "netflix",      is_older(5))
    trash(home, "haskell-cafe", is_older(1))

    -- archive rules
    archive(home, "INBOX", is_expired(7))
    move(home,    "slug",    "slug/archive",    is_expired(15))
    move(home,    "billing", "billing/archive", is_expired(15))


    -- I only need a few rules for work.
    trash(work, "issue",  is_older(7))
    trash(work, "linode", is_older(7))
    trash(work, "daily",  is_older(0))
end

-- This function takes a mutt config file and a variable name,
-- and causes mutt to parse the file and output the value of the variable.
-- the function returns this value.
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

-- This helper function lets one copy messages
-- between two folders by name, rather than value,
-- and takes a function callback (f) to act as a "predicate".
function copy(imap, from_name, to_name, f)
    local from = imap[from_name]
    local to   = imap[to_name]
    from:copy_messages(to, f(from))
end

-- This is almost identical to copy(), except it moves messages
-- instead.
function move(imap, from_name, to_name, f)
    local from = imap[from_name]
    local to   = imap[to_name]
    from:move_messages(to, f(from))
end

-- Here, I use the verb "archive", as I use gmail's imap.
-- Deleting mail from a label (or inbox) == archiving it.
function archive(imap, from_name, f)
    local from = imap[from_name]
    from:delete_messages( f(from) )
end

-- Trashing mail is the same as moving it to the Trash folder.
function trash(imap, name, f)
    move(imap, name, '[Gmail]/Trash', f)
end

-- This is a shortcut for removing mail regardless of which label(es) it is in.
function trash_all(imap, f)
    trash(imap, '[Gmail]/All Mail', f)
end


-- The following are predicate function builders,
-- which is to say they are functions which return functions which return a set of messages to act on.
-- There is nothing special about '_', it is just the 'from' folder object (c.f. move() and copy()).

-- An ignored message is something older than age and unseen (unread).
function is_ignored(age)
    return function (_) return _:is_older(age) * _:is_unseen() end
end

-- This matches all messages older than age and unflagged (unstarred in gmail terms)
function is_older(age) 
    return function (_) return _:is_older(age) * _:is_unflagged() end 
end

-- Same, except ignores unseen messages. Used on the inbox, slug, and billing folders/labels.
function is_expired(age)
    return function (_) return _:is_older(age) * _:is_seen() * _:is_unflagged() end
end


main() -- and now we begin executing the meat of the script.
