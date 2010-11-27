HOST := $(shell hostname -s)
DISPLAY ?= :0.0
DROPBOX ?= ~/.local/Dropbox

help:
	@echo targets: fixperms, schedule, pwsafe-merge
	@echo HOST = $(HOST)

.pwsafe.dat:
	scp lofn:.pwsafe.dat .
	make fixperms

.Xdefaults: .Xdefaults.tt 
	xrdb -cpp ttpp -n $< > $@
	xrdb -load $@

.msmtprc: .msmtprc.tt .emailrc
	ttpp -DACCOUNT_FILE=.emailrc $< > $@
	chmod 600 $@

.procmailrc: .procmailrc@$(HOST)
	ln -s $< $@

.procmailrc@%:
	touch $@

fixperms:
	-chmod -Rc +x ~/bin ~/.xinitrc
	-chmod -Rc go-wrx ~/.pwsafe.dat ~/pim ~/.msmtprc ~/.getmail ~/.emailrc
	-sudo chown -Rc $(USER):$(USER) $(HOME)

schedule:
	@rem -q

pwsafe-merge:
	scp lofn:.pwsafe.dat pwsafe
	pwsafe --merge pwsafe
	rm pwsafe
	scp .pwsafe.dat lofn:


docs:
	-rm $@
	ln -fs $(DROPBOX)/Documents $@

pics:
	-rm $@
	ln -fs $(DROPBOX)/Photos $@

pub:
	-rm $@
	ln -fs $(DROPBOX)/Public $@

desk:
	-rm $@
	ln -fs $(DROPBOX)/Desktop $@

mus:
	-rm $@
	ln -fs $(DROPBOX)/Music $@

vid:
	-rm $@
	ln -fs $(DROPBOX)/Video $@

tmp:
	-mkdir $@

folders=docs pics pub desk mus vid tmp

folders: $(folders)


.PHONY: schedule fixperms help pwsafe-merge folders $(folders)
