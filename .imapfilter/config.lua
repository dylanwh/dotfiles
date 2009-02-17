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
	return box:is_seen() * box:is_older(3)
end

function delete_old(imap, from)
	from = imap[ from or 'INBOX' ]
	from:delete_messages(is_old(from))
end

local home  = IMAP( netrc["smtp.gmail.com"] )

delete_old(home, 'inbox')
