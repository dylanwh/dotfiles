{
  config,
  pkgs,
  ...
}:

let
  c = config.selenized.srgbColors;
in
{
  programs.alacritty = {
    enable = true;

    settings = {
      terminal.shell = {
        program = "${pkgs.fish}/bin/fish";
        args = [ "-l" ];
      };

      window = {
        decorations = "Buttonless";
        option_as_alt = "Both";
        opacity = 1.0;
      };

      font = {
        normal.family = "SauceCodePro Nerd Font Mono";
        size = 14.0;
      };

      cursor = {
        style.blinking = "Never";
        unfocused_hollow = true;
      };

      mouse.hide_when_typing = true;

      bell.duration = 0;

      scrolling.history = 10000;

      colors = {
        primary = {
          background = c.bg_0;
          foreground = c.fg_0;
        };
        selection = {
          background = c.bg_2;
          text = "CellForeground";
        };
        normal = {
          black = c.bg_1;
          red = c.red;
          green = c.green;
          yellow = c.yellow;
          blue = c.blue;
          magenta = c.magenta;
          cyan = c.cyan;
          white = c.dim_0;
        };
        bright = {
          black = c.bg_2;
          red = c.br_red;
          green = c.br_green;
          yellow = c.br_yellow;
          blue = c.br_blue;
          magenta = c.br_magenta;
          cyan = c.br_cyan;
          white = c.fg_1;
        };
      };

      keyboard.bindings = [
        # Font size
        {
          key = "Key0";
          mods = "Command";
          action = "ResetFontSize";
        }
        {
          key = "Plus";
          mods = "Command";
          action = "IncreaseFontSize";
        }
        {
          key = "Equals";
          mods = "Command";
          action = "IncreaseFontSize";
        }
        {
          key = "Minus";
          mods = "Command";
          action = "DecreaseFontSize";
        }

        # Clipboard
        {
          key = "C";
          mods = "Command";
          action = "Copy";
        }
        {
          key = "V";
          mods = "Command";
          action = "Paste";
        }

        # Fullscreen
        {
          key = "Return";
          mods = "Command";
          action = "ToggleFullscreen";
        }

        # New window
        {
          key = "N";
          mods = "Command|Shift";
          action = "SpawnNewInstance";
        }
        {
          key = "N";
          mods = "Command";
          action = "None";
        }

        # Disable tabs
        {
          key = "T";
          mods = "Command";
          action = "None";
        }
        {
          key = "W";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key1";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key2";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key3";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key4";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key5";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key6";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key7";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key8";
          mods = "Command";
          action = "None";
        }
        {
          key = "Key9";
          mods = "Command";
          action = "None";
        }
        {
          key = "Right";
          mods = "Command";
          action = "None";
        }
        {
          key = "Left";
          mods = "Command";
          action = "None";
        }
      ];
    };
  };
}
