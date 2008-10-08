HOME = os.getenv "HOME"
package.path = HOME ..  "/.imapfilter/?.lua"
require "util"

---------------
--  Options  --
---------------
options.timeout = 120
options.subscribe = true

----------------
--  Accounts  --
----------------
function is_old(box)
	return box:is_seen() * box:is_older(7)
end

function archive_old(imap, from, to)
	from = imap[ from or 'inbox' ]
	to   = imap[ to   or 'archive']
	from:move_messages(to, is_old(from))
end


local home  = IMAP( netrc["lofn.hardison.net"] )
local work  = IMAP( netrc["r-stream.com"] )

do
	local inbox = work['inbox']
	local junk = inbox:contain_subject("batch check data")
		+ inbox:contain_subject("SugarCRM Case - RETURN PENDING")
		+ inbox:contain_subject("YouSendIt File Sent Notification")
		+ inbox:contain_subject("Portal data check for the week ending")
	local errors = inbox:contain_subject("DB Error") + inbox:contain_subject("System Error")
	local function sts(box) return box:contain_subject("salestenders Discrepancy wingstop") end


	inbox:delete_messages( junk )
	inbox:move_messages( work['errors'], errors)
	inbox:move_messages(work.salestenders, sts(inbox))
	work.archive:move_messages(work.salestenders, sts(work.archive))
end

archive_old(work, 'inbox', 'archive')
archive_old(home, 'inbox', 'archive')
