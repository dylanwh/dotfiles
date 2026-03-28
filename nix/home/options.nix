{ lib, ... }:

{
  options.terminal.fontSize = lib.mkOption {
    type = lib.types.float;
    default = 16.0;
    description = "Font size for terminal emulators.";
  };

  options.browser.fontSize = lib.mkOption {
    type = lib.types.str;
    default = "16pt";
    description = "Font size for web browsers.";
  };
}
