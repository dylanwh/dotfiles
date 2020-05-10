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
    local Junk      = server['Junk Mail']

    SaneNews:contain_from("notifications@github.com"):move_messages(GitHub)
    SaneCC:contain_from("notifications@github.com"):move_messages(GitHub)
    Archive:contain_from("notifications@github.com"):move_messages(GitHub)

    local github_at = GitHub:contain_body("@dylanwh")
    github_at:mark_flagged()
    github_at:move_messages(INBOX)
    Nest:is_older(1):delete_messages()

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

    GitHub:is_older(60):move_messages(Trash)

    INBOX:contain_from("notify@twitter.com"):move_messages(Trash)
    SaneNews:contain_from("builds@travis-ci.com"):move_messages(Trash)

    local old_netflix_recs = SaneLater:contain_from("info@mailer.netflix.com") * SaneLater:is_older(14)
    old_netflix_recs:move_messages(Trash)
    SaneLater:contain_from("ebay@reply.ebay.de"):move_messages(Junk)

    local old_fedex = INBOX:contain_from("fedex.com") * INBOX:contain_subject("Your package ") * stale
    old_fedex:move_messages(Trash)

    local capone_withdraw = INBOX:contain_from("capitalone@notification.capitalone.com") * INBOX:contain_subject("Withdrawal Notice") * INBOX:is_seen()
    capone_withdraw:move_messages(Archive)

    SaneNews:contain_from("dev-apps-bugzilla-bounces@lists.mozilla.org"):move_messages(Trash)

    move_cpan(server, INBOX)
    move_cpan(server, Archive)

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

function move_cpan(server, box)
  local cpan = box:contain_field("X-Original-Delivered-to", "dhardison@cpan.org")
  cpan:move_messages(server['CPAN'])
end

local fastmail = IMAP {
    server = 'imap.fastmail.com',
    username = "dylan@hardison.net",
    password = io.open(os.getenv("HOME") .. "/.fastmail_password", "r"):read(),
    ssl = 'tls1.2',
}

main(fastmail)
