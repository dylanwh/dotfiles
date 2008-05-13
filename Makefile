# foo
help:
	@echo targets: fixperms, schedule

fixperms:
	chmod -Rc +x ~/bin ~/.xinitrc
	chmod -Rc go-wrx ~/.netrc ~/tmp ~/.pwsafe.dat ~/pim

schedule:
	@rem -q

.pwsafe.dat:
	scp lofn.sinedev.org:.pwsafe.dat .
	make fixperms

.PHONY: schedule fixperms help
