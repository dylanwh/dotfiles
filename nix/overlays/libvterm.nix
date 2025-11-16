final: prev: {
  libvterm = prev.stdenv.mkDerivation rec {
    pname = "libvterm";
    version = "0.99.7";

    src = prev.fetchurl {
      url = "mirror://sourceforge/libvterm/${pname}-${version}.tar.gz";
      sha256 = "10gaqygmmwp0cwk3j8qflri5caf8vl3f7pwfl2svw5whv8wkn0k2";
    };

    preInstall = ''
      mkdir -p $out/include $out/lib
    '';

    postPatch = ''
      substituteInPlace Makefile \
      --replace-fail "gcc" "${prev.stdenv.cc.targetPrefix}cc" \
      --replace-fail "ldconfig" "" \
      --replace-fail "/usr" "$out" \
      --replace-fail "-lutil" "-lutil -L${prev.ncurses}/lib -lncurses `${prev.stdenv.cc.targetPrefix}pkg-config --libs glib-2.0`"
      substituteInPlace vterm.c \
          --replace-fail '#include <pty.h>' '#include <util.h>'

      makeFlagsArray+=("PKG_CFG=`${prev.stdenv.cc.targetPrefix}pkg-config --cflags glib-2.0` ")
    '';

    # For headers
    propagatedBuildInputs = [ prev.glib ];

    strictDeps = true;

    nativeBuildInputs = [ prev.pkg-config ];
    buildInputs = [ prev.ncurses ];

    meta = with prev.lib; {
      homepage = "http://libvterm.sourceforge.net/";
      description = "Terminal emulator library to mimic both vt100 and rxvt";
      license = licenses.gpl2Plus;
      platforms = platforms.all;
    };
  };
}
