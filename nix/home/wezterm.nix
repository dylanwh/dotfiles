{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".config/wezterm".source = ../../wezterm;
}
