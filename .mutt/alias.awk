#!/usr/bin/awk -f

BEGIN {
	FS=": "
}

/^\s*#/ { next }

/nick: / {
	nick = $2
	name = 0
	email = 0
}

/^name: / { name=$2 }
/^email: / { email=$2 }

nick && name && email {
	split(email, emails, /[ \t]*,[ \t]*/)

	for (i in emails) {
		if (i > 1) {
			n = nick i
		} else {
			n = nick
		}
		printf("alias %s \"%s\" <%s>\n", n, name, emails[i]);
	}
	nick = 0
	name = 0
	email = 0
}
