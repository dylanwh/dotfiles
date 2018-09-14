.PHONY: all dirs

PATH  := $(HOME)/bin:$(PATH)
HOST  := $(shell hostname -s)
GITHUB_USER := dylanwh

XDG_DATA_HOME   ?= $(HOME)/.local/share
XDG_CONFIG_HOME ?= $(HOME)/.config
XDG_CACHE_HOME  ?= $(HOME)/.cache

XDG_CONFIG_HOME := $(patsubst $(HOME)/%,%,$(XDG_CONFIG_HOME))
XDG_CACHE_HOME  := $(patsubst $(HOME)/%,%,$(XDG_CACHE_HOME))
XDG_DATA_HOME   := $(patsubst $(HOME)/%,%,$(XDG_DATA_HOME))

-include $(XDG_CACHE_HOME)/user-dirs.mk

all: .emacs.d $(XDG_CONFIG_HOME)/base16-shell .ssh/authorized_keys

$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs $(XDG_CACHE_HOME) Makefile
	@sed '/^#/ d; s|$$HOME/||g; s/"//g;' $< > $@

$(XDG_CONFIG_HOME)/base16-shell: $(XDG_CONFIG_HOME)
	git clone https://github.com/chriskempson/base16-shell.git $@

.emacs.d:
	git clone https://github.com/syl20bnr/spacemacs $@

.ssh:
	mkdir -m 755 .ssh

.ssh/authorized_keys: .ssh
	curl https://github.com/$(GITHUB_USER).keys > $@
