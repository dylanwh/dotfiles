-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.imapfilter/?.lua"
local callback = require "callback"

local fastmail = IMAP {
    server = 'mail.messagingengine.com',
    username = 'dylan@hardison.net',
    password = io.popen("security find-generic-password -s fastmail -w"):read(),
    ssl = 'tlvs1',
}

local mozilla = IMAP {
    server = 'imap.gmail.com',
    username = 'dhardison@mozilla.com',
    password = io.popen("security find-generic-password -s mozilla -w"):read(),
    ssl = 'tlvs1',
}

function ignore(mbox)
end

function move_messages_by_year(account, folder, year)
   local mbox = account[folder]
   local msgs = mbox:sent_since("01-Jan-" .. year) * mbox:sent_before("01-Jan-" .. (year+1))
   local year_folder = folder .. "/" .. year
   if not account[year_folder] then
      account:create_mailbox(year_folder)
   end
   msgs:move_messages( account[year_folder] );
end

for year = 2002, 2015 do
   move_messages_by_year(fastmail, "Archive", year)
end

local mailinglists    = {
    act               = { fastmail, "act.mongueurs.net",                     },
    slug              = { fastmail, "slug.suncoastlug.org",                  },
    phs               = { fastmail, "Pinellas-Hack-Shack-list.meetup.com", delete_older = 7 },
    boardgames        = { fastmail, "boardgames@406-list.meetup.com"         },
    ["slug/announce"] = { fastmail, "slug-announce.suncoastlug.org", age = 1 },
    mongers           = { fastmail, "pm_groups.pm.org", age = 1              },
    suckless          = { fastmail, "dev.suckless.org", delete_older = 1     },
    webdev            = { mozilla, "dev-webdev.lists.mozilla.org"            },
    bugzilla          = { mozilla, "dev-apps-bugzilla.lists.mozilla.org",    },
    autotools         = { mozilla, "auto-tools.mozilla.com"                  },
    devplatform       = { mozilla, "dev-platform@lists.mozilla.org"          },
}

for folder, action in pairs(mailinglists) do
    local account, list_id = unpack(action)
    local listmail = account.INBOX:contain_field("List-Id", list_id)
    if not account[folder] then
       account:create_mailbox(folder)
    end
    if action.age then
       listmail = listmail * account.INBOX:is_older(action.age)
    end
    listmail:move_messages(account[folder])
    if action.delete_older then
       local oldmail = account[folder]:is_older(action.delete_older)
       oldmail:delete_messages()
    end
end

function archive(src, dest)
    local old_msgs = src:is_seen() * src:is_unflagged() * src:is_older(30)
    old_msgs:move_messages(dest)
end

do
   local amazon_from = {
      "ship-confirm@amazon.com",
      "auto-confirm@amazon.com",
      "no-reply@amazon.com",
      "prime@amazon.com"
   }

   local amazons
   for _, email in ipairs(amazon_from) do
      local v = fastmail.INBOX:contain_from(email)
      if amazons then
         amazons = amazons + v
      else
         amazons = v
      end
   end
   amazons:move_messages(fastmail.amazon)
end

if not fastmail.noreply then fastmail:create_mailbox("noreply") end
fastmail.INBOX:contain_from("noreply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("noreply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("do_not_reply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("no-reply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("no_reply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("donotreply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("do.not.reply"):move_messages(fastmail.noreply)
fastmail.INBOX:contain_from("DO-NOT-REPLY"):move_messages(fastmail.noreply)

fastmail.INBOX:contain_from("replies@seeclickfix.com"):delete_messages()

do
   local fbmail = fastmail.INBOX:contain_from("facebookmail.com")
   local meetmail = fastmail.INBOX:contain_from("info@meetup.com")
   local twmail = fastmail.INBOX:contain_from("notify@twitter.com")
   local bounces = fastmail.INBOX:contain_from("bounces@")

   fbmail:delete_messages()
   meetmail:delete_messages()
   twmail:delete_messages()
   bounces:move_messages(fastmail.Archive)
end

function get_email(s)
    local m = s:match("<([^>]+)>")
    if m then
        return m
    else
       m = s:match("From: (.+)")
       if m then
          return m
       else
          return s
       end
    end
end

do
   local box = mozilla.INBOX
   local seen_or_old = (box:is_seen() + box:is_older(2))
   local crons =  seen_or_old * box:contain_field("List-Id", "cron-bugzilla.mozilla.com")
   crons:delete_messages()

   local sentry = ( seen_or_old
                       * box:contain_subject("[Sentry] [BMO]")
                       * box:contain_from("root@localhost.webapp") )
   sentry:delete_messages()

   local pto = seen_or_old * box:contain_subject("PTO notification from")
   if not mozilla.pto then
      mozilla:create_mailbox("pto")
   end
   pto:move_messages(mozilla.pto)
end



-- clean up things
function cleanup(dir, rule, age)
   local msgs = rule(dir)
   if age then
      msgs = msgs * dir:is_older(age)
   end
   msgs:delete_messages()
end

function contain_from(from)
    return function (dir)
        return dir:contain_from(from)
    end
end

local cleanup_rules = {
   { contain_from("linkedin.com"),                 age = 0 },
   { contain_from("@pearltrees.com"),               age = 0 },
   { contain_from("CostcoNews@online.costco.com"),  age = 0 },
   { contain_from("email.campaign@sg.booking.com"), age = 0 },
   { contain_from("googleplay-noreply@google.com"), age = 0 },
   { contain_from("moveon.org"),                    age = 1 },
   { contain_from("nfo@democracyforAmerica.com"),   age = 0 },
   { contain_from("nfo@dga.net"),                   age = 0 },
   { contain_from("no-reply@duolingo.com"),         age = 0 },
   { contain_from("no-reply@endomondo.com"),        age = 0 },
   { contain_from("no-reply@twitch.tv"),            age = 1 },
   { contain_from("noreply@fitbit.com"),            age = 1 },
   { contain_from("noreply@soylentnews.org"),       age = 1 },
   { contain_from("noreply@youtube.com"),           age = 0 },
   { contain_from("pages.plusgoogle.com"),          age = 1 },
   { contain_from("targetnews@e.target.com"),       age = 0 },
   { contain_from("email.vzwshop.com"),             age = 1 },
   { contain_from("myfitnesspal.com"),              age = 1 },
   { contain_from("peopleforbikes.org")                     },
   { contain_from(".codeweavers.com") },
}

for _, rule in ipairs(cleanup_rules) do
   local f = unpack(rule)
   cleanup(fastmail.INBOX, f, rule.age)
   cleanup(fastmail.noreply, f, rule.age)
end

fastmail.noreply:contain_subject("Payment"):move_messages(fastmail.billing)

do
   mozilla.bugmail:contain_field("X-Bugzilla-Type", "request"):move_messages(mozilla['bugmail/request'])

   local nag = mozilla.bugmail:contain_field("X-Bugzilla-Type", "nag") * mozilla.bugmail:is_older(1)
   nag:delete_messages()

   local notme = mozilla.bugmail:is_unseen()
      - mozilla.bugmail:contain_field("X-Bugzilla-Assigned-To", "dylan@mozilla.com")
      - mozilla.bugmail:contain_field("X-Bugzilla-Assigned-To", "nobody@mozilla.org")
   notme:mark_seen()
end

(mozilla.bugmail:is_seen() * mozilla.bugmail:is_older(1)):delete_messages()
mozilla.bugmail:is_older(7):delete_messages()
mozilla.github:is_older(7):delete_messages()
mozilla.webdev:is_older(28):delete_messages()

archive(mozilla.INBOX, mozilla.Archive)
archive(fastmail.INBOX, fastmail.Archive)
archive(fastmail.billing, fastmail["billing/archive"])
archive(fastmail.noreply, fastmail.Archive)
