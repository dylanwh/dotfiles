-- For the purposes of self-documenting code, we define a "main" function
-- which we call at the end of the file. This way the meat
-- of the code comes first, and the specific details come later.

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.imapfilter/?.lua"

function sentry(server, mbox)
  local sentry_prod   = mbox:contain_field('List-id', 'bugzilla-production-aws.operations.localhost')
  local sentry_stage  = mbox:contain_field('List-id', 'bugzilla-stage-aws.operations.localhost')
  local sentry_dev    = mbox:contain_field('List-id', 'bugzilla-dev-aws.operations.localhost')

  sentry_prod:move_messages(server['mozilla.sentry.production'])
  sentry_stage:move_messages(server['mozilla.sentry.stage'])
  sentry_dev:move_messages(server['mozilla.sentry.dev'])
end

function main(server)
    local INBOX   = server.INBOX
    local Archive = server.Archive

    local must_read     = INBOX:contain_from("vhr_mozilla@myworkday.com")
                        + INBOX:contain_from("nintendo-noreply@nintendo.net")
    local readmail      = INBOX:is_seen()
    local mozmail       = INBOX:contain_field('X-Forwarded-To', 'dylan@mozilla.hardison.net')
    local not_important = INBOX:is_unflagged()
    local docs_mail     = INBOX:contain_field('X-Gm-Message-State', "docs-share")
    local stale         = INBOX:is_older(7)
    local github        = INBOX:contain_from('notifications@github.com')

    sentry(server, INBOX);
    sentry(server, server['mozilla.lists'])
    sentry(server, Archive)

    local trash_list = {
        (must_read * readmail),
        (github * INBOX:is_older(1)),
        junk(INBOX),
        junk(Archive)
    }

    for _, msgs in ipairs(trash_list) do
        msgs:move_messages(server.Trash)
    end

    is_stale_alert(INBOX):move_messages(server.alerts)

    docs_mail:move_messages(server.gdocs)

    -- local move_to_mozilla_list = ( listmail * mozmail ) * not_important * stale
    -- move_to_mozilla_list:move_messages(server['mozilla.lists'])

    local move_to_mozilla = (mozmail * (readmail + stale) * not_important)
    move_to_mozilla:move_messages(server.mozilla)

    local archive = ( readmail * stale * not_important )
    archive:move_messages(server.Archive)

    INBOX:is_older(30):mark_seen()

end

function junk(m)
    return (
           m:contain_subject("Your receipt from Apple.")
         + m:contain_from("noreply@glassdoor.com")
         + m:contain_from("email@salamanderresort.com")
         + m:contain_from("email@salamanderhotels.com")
         + m:contain_from("andrewgillum.com")
         + m:contain_from("pearltrees.com")
         + m:contain_from("mail@info.adobesystems.com")
         + m:contain_from("jeff@jeffbrandesforstatesenate.com")
         + m:contain_from("info@sandersinstitute.com")
         + m:contain_from("Applebees@Applebees.fbmta.com")
         + m:contain_from("CheapOair@myCheapOair.com")
         + m:contain_from("Cloud9 Community summary")
         + m:contain_from("Expedia@expediamail.com")
         + m:contain_from("backstage@email.universalorlando.com")
         + m:contain_from("brian.dehaaff@aha.io")
         + m:contain_from("capitalone@capitaloneemail.com")
         + m:contain_from("democraticparty@democrats.org")
         + m:contain_from("email@washingtonpost.com")
         + m:contain_from("facebookmail.com")
         + m:contain_from("googlestore-noreply@google.com")
         + m:contain_from("grassroots@fladems.com")
         + m:contain_from("floridadems.org")
         + m:contain_from("hi@meh.com")
         + m:contain_from("campaign@jeremyring.com")
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
         + m:contain_from("reply@baycare-email.org")
         + m:contain_from("reply@email.stitchfix.com")
         + m:contain_from("reply@email.stitchfix.com")
         + m:contain_from("reply@rs.email.nextdoor.com")
         + m:contain_from("reply@workingfamilies.org")
         + m:contain_from("spectrum@emailinfo.spectrum.com")
         + m:contain_from("starzapp@em.starz.com")
         + m:contain_from("stitchfix@email.stitchfix.com")
         + m:contain_from("team@mint.com")
         + m:contain_from("usmail@expediamail.com")
         + m:contain_from("discover@airbnb.com")
         + m:contain_from("info@peopleforbikes.org")
         + m:contain_from("glassdoor.com")
         + m:contain_from("Chase@emails.chase.com")
         + m:contain_from("googleplay-noreply@google.com")
         + m:contain_from("noreply@mail.23andme.com")
         + m:contain_from("Mozilla@e.mozilla.org")
         + m:contain_from("no-reply@duolingo.com")
         + m:contain_from("team@mbfforcongress.com")
         + m:contain_from("backstage@messages.universalorlando.com")
         + m:contain_to("dylan+bob@hardison.net")
         + m:contain_to("dylan+disney@hardison.net")
         + m:contain_to("dylan+dmp@hardison.net")
         + m:contain_from("campaign@marybarzeeflores.com")
    )
end

function is_stale_alert(m)
    local sel = (
          m:contain_from("VZWMail@ecrmemail.verizonwireless.com")
        + m:contain_from("auto-confirm@amazon.com")
        + m:contain_from("info@meetup.com")
        + m:contain_from("mcinfo@ups.com")
        + m:contain_from("messaging-digest-noreply@linkedin.com")
        + m:contain_from("no-reply@alertsp.chase.com")
        + m:contain_from("notify@twitter.com")
        + m:contain_from("shipment-tracking@amazon.com")
        + m:contain_from("wiki@mozilla.com")
        + m:contain_from("noreply@email.apple.com")
        + m:contain_from("bot@heroku.com")
        + m:contain_from("w3@achievacu.com")
        + m:contain_from("microsoftfamily@microsoft.com")
        + m:contain_subject("PTO notification from")
        + m:contain_subject("moco-ldap-check")
        + m:contain_subject("no-reply@dtdg.co")
    )

    return sel * (m:is_older(2) + m:is_seen())
end

function is_old_nag(m)
    return m:contain_field('X-Bugzilla-Type', 'nag') * m:is_older(1)
end

local fastmail = IMAP {
    server = 'mail.messagingengine.com',
    username = "dylan@hardison.net",
    password = io.open(os.getenv("HOME") .. "/.fastmail_password", "r"):read(),
    ssl = 'auto',
}

main(fastmail)
