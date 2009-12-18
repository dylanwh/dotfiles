HOST := $(shell hostname -s)

help:
	@echo targets: fixperms, schedule, pwsafe-merge
	@echo HOST = $(HOST)

.pwsafe.dat:
	scp lofn:.pwsafe.dat .
	make fixperms

.Xdefaults: .Xdefaults.tt 
	xrdb -cpp ttpp -n $< > $@
	xrdb -load $@

.procmailrc: .procmailrc@$(HOST)
	ln -s $< $@

.procmailrc@%:
	touch $@

fixperms:
	-chmod -Rc +x ~/bin ~/.xinitrc
	-chmod -Rc go-wrx ~/.pwsafe.dat ~/pim ~/.msmtprc ~/.getmail
	-sudo chown -Rc $(USER):$(USER) $(HOME)

schedule:
	@rem -q

pwsafe-merge:
	scp lofn:.pwsafe.dat pwsafe
	pwsafe --merge pwsafe
	rm pwsafe
	scp .pwsafe.dat lofn:

.PHONY: schedule fixperms help pwsafe-merge
