-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.imapfilter/?.lua"


function main(account)
    local INBOX     = account.INBOX
    local Trash     = account.Trash
    local Archive   = account.Archive
    local SaneNews  = account['+SaneNews']
    local SaneLater = account['+SaneLater']
    -- local SaneCC    = account['+SaneCC']
    local GitHub    = account['GitHub']
    -- local Nest      = account['Nest']
    -- local Billing   = account['Billing']
    local Junk      = account['Junk Mail']
    local GMail     = account['GMail']

    function move_vs_category(box, category)
        box:contain_field("X-ME-VSCategory", category):move_messages(Junk)
    end

    local sanebox       = INBOX:contain_from("message-digest@sanebox.com") + INBOX:contain_from("reports@sanebox.com")
    local toggl         = INBOX:contain_subject("Toggl weekly report")
    local stale         = INBOX:is_older(1)
    local stale_sanebox = sanebox * stale
    local stale_toggl   = toggl   * stale
    stale_sanebox:delete_messages()
    stale_toggl:delete_messages()

    local more_toggl = Archive:contain_subject("Toggl weekly report")
    more_toggl:delete_messages()

    see_unseen_if_old(SaneLater)
    see_unseen_if_old(SaneNews)
    see_unseen_if_old(Archive)
    see_unseen_if_old(GMail)

    GitHub:is_older(60):move_messages(Trash)

    local later_ignore = {
      "HeyGary@SFBags.com",
      "eero@delighted.com",
      "invites@mailer.surveygizmo.com",
      "mail@floridasportfishing.com",
      "no-reply@customers.instacartemail.com",
      "no-reply@duolingo.com",
      "no-reply@lyftmail.com",
      "orders@instacart.com",
      "community@email.withings.com",
    }

    local later_trash = any_from(SaneLater, later_ignore) * SaneLater:is_older(1)
    later_trash:move_messages(Trash)

    SaneLater:is_older(30):move_messages(Archive)

    local archive_inbox =  INBOX:is_older(30) * INBOX:is_unflagged() * INBOX:is_seen()
    archive_inbox:move_messages(Archive)
end

function see_unseen_if_old(box)
    local unseen_old = box:is_older(30) * box:is_unseen()
    unseen_old:mark_seen()
end

function any_from(box, from_list)
    local from = nil
    for _, f in ipairs(from_list) do
        if from == nil then
            from = box:contain_from(f)
        else
            from = from + box:contain_from(f)
        end
    end
    return from
end


local file = io.open(os.getenv("HOME") .. "/.fastmail_password", "r")
local password
if file then
    password = file:read("*line")
end

local fastmail = IMAP {
    server = 'imap.fastmail.com',
    username = "dylan@hardison.net",
    password = password,
    ssl = 'tls1.2',
}

main(fastmail)
