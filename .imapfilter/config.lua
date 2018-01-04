-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.imapfilter/?.lua"
local callback = require "callback"

local fastmail = IMAP {
    server = 'mail.messagingengine.com',
    username = "dylan@hardison.net",
    password = io.popen("security find-generic-password -s mutt -w"):read(),
    ssl = 'auto',
}

function trash(messages)
    messages:move_messages(fastmail.Trash)
end


function junk(m)
    return ( 
           m:contain_subject("Your receipt from Apple.")
         + m:contain_from("CheapOair@myCheapOair.com")
         + m:contain_from("Cloud9 Community summary")
         + m:contain_from("backstage@email.universalorlando.com")
         + m:contain_from("brian.dehaaff@aha.io")
         + m:contain_from("democraticparty@democrats.org")
         + m:contain_from("email@washingtonpost.com")
         + m:contain_from("facebookmail.com")
         + m:contain_from("googlestore-noreply@google.com")
         + m:contain_from("grassroots@fladems.com")
         + m:contain_from("hulu@hulumail.com")
         + m:contain_from("info@BernieSanders.com")
         + m:contain_from("info@democracyforamerica.com")
         + m:contain_from("info@e.execucar.com")
         + m:contain_from("info@e.supershuttle.com")
         + m:contain_from("info@mailer.netflix.com")
         + m:contain_from("info@ourrevolution.com")
         + m:contain_from("info@send.grammarly.com")
         + m:contain_from("marketing@supershuttle.com")
         + m:contain_from("microsoftrewards@email.microsoftrewards.com")
         + m:contain_from("moveon-help@list.moveon.org")
         + m:contain_from("news@kayak.com")
         + m:contain_from("newsletter@circleci.com")
         + m:contain_from("newsletter@news.strava.com")
         + m:contain_from("no-reply@strava.com")
         + m:contain_from("nytimes@email.newyorktimes.com")
         + m:contain_from("payments@mint.com")
         + m:contain_from("reply@email.stitchfix.com")
         + m:contain_from("reply@email.stitchfix.com")
         + m:contain_from("reply@rs.email.nextdoor.com")
         + m:contain_from("reply@workingfamilies.org")
         + m:contain_from("starzapp@em.starz.com")
         + m:contain_from("team@mint.com")
         + m:contain_from("usmail@expediamail.com")
         + m:contain_to("dylan+bob@hardison.net")
         + m:contain_to("dylan+disney@hardison.net")
         + m:contain_to("dylan+dmp@hardison.net")
    )
end

function stale_alerts(m)
    local sel = m:contain_from("shipment-tracking@amazon.com")
              + m:contain_from("notify@twitter.com")
              + m:contain_from("mcinfo@ups.com")
              + m:contain_from("wiki@mozilla.com")
              + m:contain_from("auto-confirm@amazon.com")
              + m:contain_from("no-reply@alertsp.chase.com")

    return sel * (m:is_older(7) + m:is_seen())
end

function is_old_nag(m)
    return m:contain_field('X-Bugzilla-Type', 'nag') * m:is_older(1)
end

local must_read = fastmail.INBOX:contain_from("vhr_mozilla@myworkday.com")
                + fastmail.INBOX:contain_from("nintendo-noreply@nintendo.net")
local readmail = fastmail.INBOX:is_seen()
local mozmail = fastmail.INBOX:contain_field('X-Forwarded-To', 'dylan@mozilla.hardison.net')
local listmail = fastmail.INBOX:contain_field('List-Id', '.')
local not_important = fastmail.INBOX:is_unflagged()
local docs_mail = fastmail.INBOX:contain_field('X-Gm-Message-State', "docs-share")
local stale = fastmail.INBOX:is_older(7)

trash(must_read * readmail)

trash(junk(fastmail.INBOX))
-- trash(junk(fastmail.Archive))
-- for year = 2002, 2015 do
--     trash(junk(fastmail['zz-archives.' .. year]))
-- end
stale_alerts(fastmail.INBOX):move_messages(fastmail.alerts)


bugmails = {
    fastmail['mozilla.bugmail.bmo'],
    fastmail['mozilla.bugmail'],
    fastmail['mozilla.bugmail.websites'],
    fastmail['bugzilla']
}
for _, bugmail in ipairs(bugmails) do
    trash( bugmail:is_older(14) )
end

trash( is_old_nag(fastmail['mozilla.bugmail']) )
trash( is_old_nag(fastmail['bugzilla']) )

docs_mail:move_messages(fastmail.gdocs)

local yesterday = fastmail.INBOX:is_older(1)
local move_to_mozilla_list = (mozmail * listmail) * not_important * yesterday
move_to_mozilla_list:move_messages(fastmail['mozilla.lists'])

local move_to_mozilla = (mozmail * (readmail + stale) * not_important)
move_to_mozilla:move_messages(fastmail.mozilla)

fastmail.mozilla:contain_field('List-Id', '.'):move_messages(fastmail['mozilla.lists'])

local archive = readmail * stale * not_important
archive:move_messages(fastmail.Archive)


