{ config, ... }:

{
  programs.firefox = {
    profiles.default = {
      id = 0;
      isDefault = true;
      path = "3vadokcd.default";
      settings = {
        # use the windows key instead of ctrl
        "ui.key.accelKey" = 91;
        "font.name.monospace.x-western" = "SauceCodePro Nerd Font Mono";
        "font.size.monospace.x-western" = builtins.floor (config.terminal.fontSize * 4.0 / 3.0);
      };
    };
  };

  home.file.".mozilla/firefox/3vadokcd.default/customKeys.json" = {
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/firefox/customKeys.json";
    force = true;
  };
}
