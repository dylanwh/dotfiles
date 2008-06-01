help:
	@echo targets: fixperms, schedule

fixperms:
	chmod -Rc +x ~/bin ~/.xinitrc
	chmod -Rc go-wrx ~/.netrc ~/tmp ~/.pwsafe.dat ~/pim

schedule:
	@rem -q

pwsafe-merge:
	scp lofn:.pwsafe.dat pwsafe
	pwsafe --merge pwsafe
	rm pwsafe
	scp .pwsafe.dat lofn:

.pwsafe.dat:
	scp lofn.sinedev.org:.pwsafe.dat .
	make fixperms

.PHONY: schedule fixperms help
