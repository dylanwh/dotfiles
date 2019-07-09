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
    for year = 2003, 2015 do
        local folder = server['zz-archives.' .. year]
        folder:select_all():move_messages(Archive)
    end
end

function is_mozilla(m)
    return m:contain_field( "X-Delivered-to","dylan@mozilla.hardison.net")
     + m:contain_from("dylan@mozilla.com")
     + m:contain_from("dhardison@mozilla.com")
     + m:contain_to("dhardison@mozilla.com")
     + m:contain_to("dylan@mozilla.com")
     + m:contain_cc("dhardison@mozilla.com")
     + m:contain_cc("dylan@mozilla.com")
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

local fastmail = IMAP {
    server = 'mail.messagingengine.com',
    username = "dylan@hardison.net",
    password = io.open(os.getenv("HOME") .. "/.fastmail_password", "r"):read(),
    ssl = 'auto',
}

main(fastmail)
