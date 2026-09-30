{
  config,
  pkgs,
  lib,
  ...
}:

let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  withNixEnv = "${config.home.homeDirectory}/.local/bin/with-nix-env ";
  ghostty = if isDarwin then pkgs.ghostty-bin else pkgs.ghostty;

  variants = builtins.fromJSON (builtins.readFile ../../selenized/variants.json);

  mkColors =
    variantName: builtins.mapAttrs (_: value: "#${builtins.elemAt value 0}") variants.${variantName};

  mkTheme =
    variantName:
    let
      c = mkColors variantName;
    in
    {
      background = c.bg_0;
      foreground = c.fg_0;
      selection-background = c.bg_2;
      palette = [
        "0=${c.bg_1}"
        "1=${c.red}"
        "2=${c.green}"
        "3=${c.yellow}"
        "4=${c.blue}"
        "5=${c.magenta}"
        "6=${c.cyan}"
        "7=${c.dim_0}"
        "8=${c.bg_2}"
        "9=${c.br_red}"
        "10=${c.br_green}"
        "11=${c.br_yellow}"
        "12=${c.br_blue}"
        "13=${c.br_magenta}"
        "14=${c.br_cyan}"
        "15=${c.fg_1}"
      ];
    };
in
{
  programs.ghostty = {
    enable = true;
    package = ghostty;
    enableFishIntegration = false;
    enableBashIntegration = false;
    enableZshIntegration = false;
    clearDefaultKeybinds = true;

    themes = {
      selenized-black = mkTheme "black";
      selenized-dark = mkTheme "dark";
      selenized-light = mkTheme "light";
      selenized-white = mkTheme "white";
    };

    settings = {
      font-family = "SauceCodePro Nerd Font Mono";
      font-size = config.terminal.fontSize;
      theme = "selenized-black";
      initial-window = true;
      quit-after-last-window-closed = false;
      cursor-style-blink = false;
      mouse-hide-while-typing = true;
      keybind = [ "global:super+semicolon=toggle_quick_terminal" ];
      quick-terminal-position = "center";
    }
    // lib.optionalAttrs isDarwin {
      command = "${withNixEnv}eshell";
      macos-option-as-alt = true;
      macos-titlebar-style = "native";
      macos-window-buttons = "hidden";
      window-colorspace = "display-p3";
    };
  };
}
