-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.imapfilter/?.lua"

function main(server)
    local INBOX     = server.INBOX
    local Trash     = server.Trash
    local Archive   = server.Archive
    local SaneNews  = server['+SaneNews']
    local SaneLater = server['+SaneLater']
    local SaneCC    = server['+SaneCC']
    local GitHub    = server['GitHub']
    local Nest      = server['Nest']
    local Billing   = server['Billing']

    SaneNews:contain_from("notifications@github.com"):move_messages(GitHub)
    SaneCC:contain_from("notifications@github.com"):move_messages(GitHub)
    GitHub:contain_body("@dylanwh"):move_messages(INBOX)
    Nest:is_older(1):delete_messages()

    local sanebox       = INBOX:contain_from("message-digest@sanebox.com") + INBOX:contain_from("reports@sanebox.com")
    local stale         = INBOX:is_older(1)
    local stale_sanebox = sanebox * stale
    stale_sanebox:delete_messages()

    see_unseen_if_old(SaneLater)
    see_unseen_if_old(SaneNews)
    see_unseen_if_old(Archive)

    GitHub:is_older(60):move_messages(Trash)

    INBOX:contain_from("notify@twitter.com"):move_messages(Trash)
    SaneNews:contain_from("builds@travis-ci.com"):move_messages(Trash)

    local old_netflix_recs = SaneLater:contain_from("info@mailer.netflix.com") * SaneLater:is_older(14)
    old_netflix_recs:move_messages(Trash)
end

function see_unseen_if_old(box)
    local unseen_old = box:is_older(30) * box:is_unseen()
    unseen_old:mark_seen()
end

local fastmail = IMAP {
    server = 'imap.fastmail.com',
    username = "dylan@hardison.net",
    password = io.open(os.getenv("HOME") .. "/.fastmail_password", "r"):read(),
    ssl = 'tls1.2',
}

main(fastmail)
