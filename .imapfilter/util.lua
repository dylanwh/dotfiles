--[[
IMAP {
    server = 'mail.hardison.net',
    username = 'dylanwh',
    password = chomp(slurp(HOME .. "/.imap_homepass")),
    ssl = 'ssl3',
}]]

netrc = { }
local file = io.open( os.getenv("HOME") .. "/.netrc")
local machine
local cmds = {
	machine = function(server) 
		machine = { server = server, ssl = 'ssl3' }
		netrc[server] = machine
	end,
	login = function (name) 
		machine.username = name
	end,
	password = function (pass) 
		machine.password = pass
	end,
}

for line in file:lines() do
	local cmd, arg = line:match("[\t ]*([%w_]+) +([^ ]+)")
	if cmd and cmds[cmd] then
		cmds[cmd](arg)
	end
end
