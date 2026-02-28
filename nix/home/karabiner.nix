{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".config/karabiner".source = ../../karabiner;
}
