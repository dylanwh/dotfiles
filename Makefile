.PHONY: all

all: .config/i3/config .config/i3status/config

%: %.tt
	@if [[ -f $@ ]]; then chmod u+w $@; fi
	ttpp $< > $@
	@chmod a-w $@


