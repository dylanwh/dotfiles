
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

-include $(XDG_CACHE_HOME)/configure.mk

define git-clone
@if [ -d $(2) ]; then \
	echo 'cd $(2) && git pull' ; \
	cd $(2) && git pull; \
else \
	echo 'git clone $(1) $(2)'; \
	git clone --depth 1 -q $(1) $(2); \
fi
@touch $(2)
endef

.PHONY: all plenv emacs ssh base16
all:    plenv emacs ssh base16
plenv:  .plenv .plenv/plugins/perl-build
emacs:  .emacs.d
ssh:    .ssh/authorized_keys
base16: $(XDG_CONFIG_HOME)/base16-shell


$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs $(XDG_CACHE_HOME) Makefile
	@sed '/^#/ d; s|$$HOME/||g; s/"//g;' $< > $@

$(XDG_CACHE_HOME)/configure.mk: $(XDG_CACHE_HOME) Makefile
	echo 'UNAME='$$(uname) > $@

$(XDG_CONFIG_HOME)/base16-shell: $(XDG_CONFIG_HOME)
	$(call git-clone,https://github.com/chriskempson/base16-shell.git,$@)

.plenv:
	$(call git-clone,https://github.com/tokuhirom/plenv.git,$@)

.plenv/plugins/perl-build: .plenv
	$(call git-clone,https://github.com/tokuhirom/Perl-Build.git,$@)

.emacs.d:
	$(call git-clone,https://github.com/syl20bnr/spacemacs,$@)

ifeq ($(UNAME),linux)
.PHONY: brew
brew: .linuxbrew/Homebrew .linuxbrew/bin/brew

.linuxbrew:
	mkdir -m 755 $@

.linuxbrew/bin: .linuxbrew
	mkdir -m 755 $@

.linuxbrew/Homebrew: .linuxbrew
	$(call git-clone,https://github.com/Homebrew/brew,$@)

.linuxbrew/bin/brew: .linuxbrew/bin
	ln -s ../Homebrew/bin/brew $@
endif

.ssh:
	mkdir -m 755 $@

$(XDG_CACHE_HOME):
	mkdir -m 755 $@

.ssh/authorized_keys: .ssh
	curl -s -o $@ https://github.com/$(GITHUB_USER).keys
	@chmod 644 $@

.ssh/config: $(sort $(wildcard .ssh/config.d/*))
	cat $^ > $@
	@chmod 644 $@
