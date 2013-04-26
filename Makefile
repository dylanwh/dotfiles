.PHONY: all dirs

XDG_DATA_HOME   ?= $(HOME)/.data
XDG_CONFIG_HOME ?= $(HOME)/.config
XDG_CACHE_HOME  ?= $(HOME)/.cache

ENSURE_DIRS = $(XDG_DATA_HOME) \
			  $(XDG_CONFIG_HOME) \
			  $(XDG_CACHE_HOME) \
			  $(XDG_CACHE_HOME)/ssh \
			  $(XDG_CACHE_HOME)/zsh \
			  $(XDG_CACHE_HOME)/vim \
			  $(XDG_CACHE_HOME)/vim/backup \
			  $(XDG_CACHE_HOME)/vim/swap \
			  $(XDG_CACHE_HOME)/vim/undo \
			  $(XDG_CACHE_HOME)/vim/view 

all: .config/i3/config .config/i3status/config dirs

dirs: $(ENSURE_DIRS)

%: %.tt
	@if [[ -f $@ ]]; then chmod u+w $@; fi
	ttpp -o $@ $<
	@chmod a-w $@

$(ENSURE_DIRS):
	mkdir -p $@
