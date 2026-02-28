{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
}
