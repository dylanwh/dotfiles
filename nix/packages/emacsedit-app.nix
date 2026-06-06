{ stdenv, lib }:

stdenv.mkDerivation {
  pname = "emacsedit-app";
  version = "1.0";

  src = ../../emacsedit-app;

  buildInputs = [ ];

  buildPhase = ''
    # Remove any pre-existing .app bundles copied from source
    rm -rf EmacsEdit-unsigned.app EmacsEdit.app

    # Build the unsigned app
    /usr/bin/osacompile -o EmacsEdit-unsigned.app main.applescript

    # Copy custom Info.plist
    cp Info.plist EmacsEdit-unsigned.app/Contents/Info.plist

    # Copy icon
    cp Emacs.icns EmacsEdit-unsigned.app/Contents/Resources/Emacs.icns

    # Set icon file reference
    /usr/bin/plutil -replace CFBundleIconFile -string "Emacs.icns" EmacsEdit-unsigned.app/Contents/Info.plist

    # Create signed copy
    cp -R EmacsEdit-unsigned.app EmacsEdit.app

    # Code sign with ad-hoc signature
    /usr/bin/codesign --force --deep --sign - EmacsEdit.app
  '';

  installPhase = ''
    mkdir -p $out/Applications
    cp -R EmacsEdit.app $out/Applications/
  '';

  meta = with lib; {
    description = "macOS app for opening files in Emacs via emacsclient";
    platforms = platforms.darwin;
  };
}
