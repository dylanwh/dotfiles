.PHONY: all dirs clean

XDG_DATA_HOME   ?= $(HOME)/.data
XDG_CONFIG_HOME ?= $(HOME)/.config
XDG_CACHE_HOME  ?= $(HOME)/.cache

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

-include $(XDG_CACHE_HOME)/user-dirs.mk


all: .config/i3/config .config/i3status/config dirs

dirs: $(ENSURE_DIRS)

$(XDG_CACHE_HOME)/user-dirs.mk: $(XDG_CONFIG_HOME)/user-dirs.dirs
	sed 's/$$HOME/$$(HOME)/g; s/"//g;' $< > $@

%: %.tt
	@if [[ -f $@ ]]; then chmod u+w $@; fi
	ttpp -o $@ $<
	@chmod a-w $@

$(ENSURE_DIRS):
	mkdir -p $@
	
clean:
	rm -f .config/i3/config .config/i3status/config
