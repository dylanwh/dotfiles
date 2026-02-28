{ config, pkgs, ... }:

let
  c = config.selenized.colors;
in
{
  programs.kitty = {
    enable = true;

    font = {
      name = "SauceCodePro Nerd Font Mono";
      size = 14.0;
    };

    actionAliases = {
      launch_tab = "launch --type=tab";
      launch_win = "launch --type=window";
      hints = "kitten hints --alphabet asdfghjklqwertyuiopzxcvbnm";
    };

    keybindings = {
      "cmd+," = "edit_config_file";
      "cmd+0" = "change_font_size all 0";
      "cmd+ctrl+," = "load_config_file";
      "cmd+e" = "open_url_with_hints";
      "cmd+g" = "focus_visible_window";
      "cmd+shift+g" = "swap_with_window";
      "cmd+h" = "hide_macos_app";
      "cmd+opt+h" = "hide_macos_other_apps";
      "cmd+l" = "next_layout";
      "cmd+m" = "minimize_macos_window";
      "cmd+shift+n" = "launch --type=os-window fish";
      "cmd+o" = "open_url_with_hints";
      "cmd+p>f" = "hints --type path --program -";
      "cmd+p>h" = "hints --type hash --program @";
      "cmd+p>l" = "hints --type line --program @";
      "cmd+p>n" = "hints --type linenum --program launch";
      "cmd+p>shift+f" = "hints --type path";
      "cmd+p>w" = "hints --type word --program @";
      "cmd+p>y" = "hints --type hyperlink --program launch";
      "cmd+r" = "start_resizing_window";
      "cmd+opt+r" = "clear_terminal reset active";
      "cmd+shift+r" = "clear_terminal reset active";
      "cmd+opt+s" = "toggle_macos_secure_keyboard_entry";
      "cmd+v" = "paste_from_clipboard";
      "cmd+plus" = "change_font_size all +2.0";
      "cmd+equal" = "change_font_size all +2.0";
      "cmd+minus" = "change_font_size all -2.0";
      "cmd+escape" = "kitty_shell overlay";
      "cmd+return" = "toggle_fullscreen";
    };

    settings = {
      # Appearance
      text_composition_strategy = "platform";
      cursor_shape_unfocused = "hollow";
      cursor_blink_interval = 0;
      cursor_trail = 0;
      cursor_trail_decay = "0.1 0.4";
      cursor_trail_start_threshold = 3;
      mouse_hide_wait = "3.0";
      inactive_text_alpha = "0.8";
      hide_window_decorations = "titlebar-only";

      # Bell
      enable_audio_bell = false;
      visual_bell_duration = "2.0 ease-in linear";
      bell_on_tab = "🔔 ";

      # Layout
      enabled_layouts = "grid,all";
      tab_bar_edge = "top";
      tab_bar_style = "hidden";
      tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{tab.last_focused_progress_percent}{index}:{title}";

      # Input
      paste_actions = "quote-urls-at-prompt,replace-dangerous-control-codes,replace-newline,confirm";
      kitty_mod = "cmd";
      clear_all_shortcuts = true;

      # Shell
      shell = "${pkgs.fish}/bin/fish";
      editor = "emacsedit";

      # Remote control
      allow_remote_control = "socket-only";
      listen_on = "unix:\${HOME}/.kitty.sock";

      # macOS
      macos_titlebar_color = "background";
      macos_option_as_alt = true;
      macos_show_window_title_in = "window";
      macos_colorspace = "default";

      # Theme colors
      foreground = c.fg_0;
      background = c.bg_0;
      background_opacity = 1;
      dynamic_background_opacity = false;
      dim_opacity = "0.625";
      selection_foreground = "none";
      selection_background = c.bg_2;
      active_tab_foreground = c.fg_1;
      active_tab_background = c.bg_2;
      inactive_tab_foreground = c.dim_0;
      inactive_tab_background = c.bg_0;
      tab_bar_background = c.bg_0;
      color0 = c.bg_1;
      color8 = c.bg_2;
      color1 = c.red;
      color9 = c.br_red;
      color2 = c.green;
      color10 = c.br_green;
      color3 = c.yellow;
      color11 = c.br_yellow;
      color4 = c.blue;
      color12 = c.br_blue;
      color5 = c.magenta;
      color13 = c.br_magenta;
      color6 = c.cyan;
      color14 = c.br_cyan;
      color7 = c.dim_0;
      color15 = c.fg_1;
    };
  };

  home.file.".config/kitty/open-actions.conf".source = ../../kitty/open-actions.conf;
  home.file.".config/kitty/launch-actions.conf".source = ../../kitty/launch-actions.conf;
}
