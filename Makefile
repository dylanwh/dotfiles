
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

define have
@echo check for $(1)
@if which $(1) 2> /dev/null > /dev/null; then \
	echo HAVE_$(1)=1 | tr a-z A-Z | tr - _; \
else \
	echo "# Don't have $(1)"; \
fi
endef

.PHONY: all plenv emacs ssh base16 fish clean_fish
all:    plenv emacs base16 fish
plenv:  .plenv .plenv/plugins/perl-build
emacs:  .emacs.d
base16: $(XDG_CONFIG_HOME)/base16-shell
ssh:
	curl -s -o Sync/ssh/authorized_keys https://github.com/$(GITHUB_USER).keys

$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs $(XDG_CACHE_HOME) Makefile
	@sed '/^#/ d; s|$$HOME/||g; s/"//g;' $< > $@

$(XDG_CACHE_HOME)/configure.mk: $(XDG_CACHE_HOME) Makefile
	@echo check uname
	@echo 'UNAME='$$(uname) > $@
	$(call have,plenv) >> $@
	$(call have,pyenv) >> $@
	$(call have,chef) >> $@
	$(call have,go) >> $@

$(XDG_CONFIG_HOME)/base16-shell: $(XDG_CONFIG_HOME) Makefile
	$(call git-clone,https://github.com/chriskempson/base16-shell.git,$@)

.plenv: Makefile
	$(call git-clone,https://github.com/tokuhirom/plenv.git,$@)

.plenv/plugins/perl-build: .plenv Makefile
	$(call git-clone,https://github.com/tokuhirom/Perl-Build.git,$@)

.emacs.d: Makefile
	$(call git-clone,https://github.com/syl20bnr/spacemacs,$@)

.ssh:
	mkdir -m 755 $@

$(XDG_CACHE_HOME):
	mkdir -m 755 $@

$(XDG_CACHE_HOME)/ssh:
	mkdir -m 755 $@

ifdef HAVE_PYENV
FISH_FILES += .config/fish/pyenv.fish
.config/fish/pyenv.fish:
	-pyenv init - --no-rehash fish | sed '/set -gx PATH/ d' > $@
endif

ifdef HAVE_PLENV
FISH_FILES += .config/fish/plenv.fish
.config/fish/plenv.fish:
	-plenv init - fish | sed '/set -gx PATH/ d' > $@
endif

ifdef HAVE_CHEF
FISH_FILES += .config/fish/chef.fish
.config/fish/chef.fish:
	-chef shell-init fish | grep -v 'set -gx PATH' > $@
endif

fish: $(FISH_FILES)
clean_fish: 
	rm -f $(FISH_FILES)


ifdef HAVE_GO
.PHONY: gore
gore:
	go get -u github.com/motemen/gore/cmd/gore
	go get -u github.com/k0kubun/pp
	go get -u github.com/mdempsky/gocode

endif
