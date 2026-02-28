{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".config/noctalia".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/noctalia";
}
