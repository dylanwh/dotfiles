{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".config/niri".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/niri";
}
