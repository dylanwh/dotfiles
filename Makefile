.PHONY: all dirs clean show-dirs show-files

XDG_DATA_HOME   ?= $(HOME)/.data
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
			  $(XDG_DATA_HOME)/vim \
			  $(XDG_DATA_HOME)/vim/backup \
			  $(XDG_DATA_HOME)/vim/swap \
			  $(XDG_DATA_HOME)/vim/undo \
			  $(XDG_DATA_HOME)/vim/view \
			  $(XDG_DESKTOP_DIR) \
			  $(XDG_DOCUMENTS_DIR) \
			  $(XDG_DOWNLOAD_DIR) \
			  $(XDG_MUSIC_DIR) \
			  $(XDG_PICTURES_DIR) \
			  $(XDG_PUBLICSHARE_DIR) \
			  $(XDG_TEMPLATES_DIR) \
			  $(XDG_VIDEOS_DIR)


GEN_FILES = $(XDG_CONFIG_HOME)/i3/config \
			$(XDG_CONFIG_HOME)/i3status/config


-include $(XDG_CACHE_HOME)/user-dirs.mk

all:  $(GEN_FILES) dirs

dirs: $(ENSURE_DIRS)

show-dirs:
	@for dir in $(ENSURE_DIRS) $(GEN_DIRS); do echo $$dir; done
	
show-files:
	@for dir in $(GEN_FILES); do echo $$dir; done

$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs $(XDG_CACHE_HOME) Makefile
	sed '/^#/ d; s|$$HOME/||g; s/"//g;' $< > $@


$(XDG_CONFIG_HOME)/i3/config: $(XDG_CONFIG_HOME)/i3/config.tt
	ttpp -Tstar -o $@ $<

$(XDG_CONFIG_HOME)/i3status/config: $(XDG_CONFIG_HOME)/i3status/config.tt
	ttpp -Tstar -o $@ $<

$(ENSURE_DIRS):
	mkdir -p $@
	
clean:
	rm -f $(GEN_FILES)
