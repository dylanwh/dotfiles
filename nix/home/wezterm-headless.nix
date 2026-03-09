{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = [ pkgs.wezterm.headless ];
  home.sessionVariables = {
    TERMINFO_DIRS = "${config.home.homeDirectory}/.nix-profile/share/terminfo";
    COLORTERM = "truecolor";
  };

}
