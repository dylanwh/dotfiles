.PHONY: all clean install
.INTERMEDIATE: $(UNSIGNED_APP)

UNSIGNED_APP = EmacsEdit-unsigned.app
SIGNED_APP = EmacsEdit.app
SCRIPT = main.applescript
PLIST = Info.plist
ICON = Emacs.icns

all: $(SIGNED_APP)

$(UNSIGNED_APP): $(SCRIPT) $(PLIST) $(ICON)
	@echo "Building $(UNSIGNED_APP)..."
	osacompile -o $(UNSIGNED_APP) $(SCRIPT)
	@echo "Copying custom Info.plist..."
	cp $(PLIST) $(UNSIGNED_APP)/Contents/Info.plist
	@echo "Copying icon..."
	cp $(ICON) $(UNSIGNED_APP)/Contents/Resources/$(ICON)
	@echo "Setting icon file reference..."
	plutil -replace CFBundleIconFile -string "$(ICON)" $(UNSIGNED_APP)/Contents/Info.plist
	@echo "Build complete: $(UNSIGNED_APP)"

$(SIGNED_APP): $(UNSIGNED_APP)
	@echo "Creating signed copy..."
	rm -rf $(SIGNED_APP)
	cp -R $(UNSIGNED_APP) $(SIGNED_APP)
	@echo "Code signing $(SIGNED_APP)..."
	codesign --force --deep --sign - $(SIGNED_APP)
	@echo "Signed: $(SIGNED_APP)"

clean:
	rm -rf $(UNSIGNED_APP) $(SIGNED_APP)

install: $(SIGNED_APP)
	@echo "Installing to /Applications..."
	rm -rf /Applications/$(SIGNED_APP)
	cp -R $(SIGNED_APP) /Applications/
	@echo "Installed to /Applications/$(SIGNED_APP)"
	@echo "You may need to run: killall Finder"
