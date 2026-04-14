{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./bash.nix
    ./emacs.nix
    ./fish.nix
    ./git.nix
    ./local-bin.nix
    ./options.nix
    ./selenized.nix
    ./ssh.nix
    ./starship.nix
    ./tmux.nix
    ./vim.nix
  ];

  programs.ripgrep = {
    enable = true;
    arguments = [ "--smart-case" ];
  };

  home.file = {
    # Dotfiles
    ".cvsignore".source = ../../cvsignore;
    ".fdignore".source = ../../fdignore;
    ".reply.pl".source = ../../reply.pl;
    ".perltidyrc".source = ../../perltidyrc;
    ".todorc".source = ../../todorc;
    ".wyrdrc".source = ../../wyrdrc;
  };

  home.sessionVariables = {
    TERMINFO_DIRS = "${config.home.homeDirectory}/.nix-profile/share/terminfo";
    COLORTERM = "truecolor";
  };

  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];
}
