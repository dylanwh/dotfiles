{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.ripgrep = {
    enable = true;
    arguments = [ "--smart-case" ];
  };

  home.file = {
    # Config directories (nix store)
    ".config/iterm2/com.googlecode.iterm2.plist".source = ../../iterm2/com.googlecode.iterm2.plist;
    ".mutt".source = ../../mutt;

    # Config directory (out-of-store, mutable at runtime)
    ".imapfilter".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/imapfilter";

    # Dotfiles
    ".cvsignore".source = ../../cvsignore;
    ".fdignore".source = ../../fdignore;
    ".msmtprc".source = ../../msmtprc;
    ".reply.pl".source = ../../reply.pl;
    ".perltidyrc".source = ../../perltidyrc;
    ".kxkbrc".source = ../../kxkbrc;
    ".todorc".source = ../../todorc;
    ".wyrdrc".source = ../../wyrdrc;
  }
  // lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".config/macos-user-key-equivalents.txt".source = ../../macos-user-key-equivalents.txt;
  };
}
