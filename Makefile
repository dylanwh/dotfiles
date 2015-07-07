.PHONY: all dirs links

PATH:=$(HOME)/bin:$(PATH)

XDG_DATA_HOME   ?= $(HOME)/.local/share
XDG_CONFIG_HOME ?= $(HOME)/.config
XDG_CACHE_HOME  ?= $(HOME)/.cache

XDG_CONFIG_HOME := $(patsubst $(HOME)/%,%,$(XDG_CONFIG_HOME))
XDG_CACHE_HOME  := $(patsubst $(HOME)/%,%,$(XDG_CACHE_HOME))
XDG_DATA_HOME   := $(patsubst $(HOME)/%,%,$(XDG_DATA_HOME))

ENSURE_DIRS = $(XDG_DATA_HOME) \
			  $(XDG_CONFIG_HOME) \
			  $(XDG_CACHE_HOME) \
			  $(XDG_CACHE_HOME)/ssh \
			  $(XDG_CACHE_HOME)/zsh \
			  $(XDG_DATA_HOME)/zsh \
			  $(XDG_DATA_HOME)/tmux \
			  $(XDG_DATA_HOME)/vim \
			  $(XDG_DATA_HOME)/vim/backup \
			  $(XDG_DATA_HOME)/vim/swap \
			  $(XDG_DATA_HOME)/vim/undo \
			  $(XDG_DATA_HOME)/vim/view \
			  $(XDG_DATA_HOME)/vim/netrw \
			  $(XDG_DESKTOP_DIR) \
			  $(XDG_DOCUMENTS_DIR) \
			  $(XDG_DOWNLOAD_DIR) \
			  $(XDG_MUSIC_DIR) \
			  $(XDG_PICTURES_DIR) \
			  $(XDG_PUBLICSHARE_DIR) \
			  $(XDG_TEMPLATES_DIR) \
			  $(XDG_VIDEOS_DIR)

ENSURE_LINKS = mnt .pwsafe.dat

-include $(XDG_CACHE_HOME)/user-dirs.mk

all:  links dirs

dirs: $(ENSURE_DIRS)

links: $(ENSURE_LINKS)

mnt:
	@for dir in /run/media/$$LOGNAME /Volumes; do \
		if [[ -d $$dir ]]; then \
			ln -svf $$dir ~/mnt; \
			break; \
		fi; \
	done

.pwsafe.dat: annex/private/pwsafe.dat
	@ln -svf $< $@

$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs $(XDG_CACHE_HOME) Makefile
	@sed '/^#/ d; s|$$HOME/||g; s/"//g;' $< > $@

$(ENSURE_DIRS):
	mkdir -p $@