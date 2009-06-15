help:
	@echo targets: fixperms, schedule, pwsafe-merge

.pwsafe.dat:
	scp lofn:.pwsafe.dat .
	make fixperms

.netrc:
	scp lofn:.netrc .
	make fixperms

.Xdefaults: .Xdefaults.tt 
	xrdb -cpp ttpp -n $< > $@
	xrdb -cpp ttpp -load $@


fixperms:
	chmod -Rc +x ~/bin ~/.xinitrc
	chmod -Rc go-wrx ~/.netrc ~/.pwsafe.dat ~/pim ~/.msmtprc ~/.getmail

schedule:
	@rem -q

pwsafe-merge:
	scp lofn:.pwsafe.dat pwsafe
	pwsafe --merge pwsafe
	rm pwsafe
	scp .pwsafe.dat lofn:

.PHONY: schedule fixperms help pwsafe-merge
