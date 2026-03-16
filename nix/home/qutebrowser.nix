{
  config,
  ...
}:

let
  # Catppuccin Mocha
  p = {
    rosewater = "#f5e0dc";
    flamingo = "#f2cdcd";
    pink = "#f5c2e7";
    mauve = "#cba6f7";
    red = "#f38ba8";
    maroon = "#eba0ac";
    peach = "#fab387";
    yellow = "#f9e2af";
    green = "#a6e3a1";
    teal = "#94e2d5";
    sky = "#89dceb";
    sapphire = "#74c7ec";
    blue = "#89b4fa";
    lavender = "#b4befe";
    text = "#cdd6f4";
    subtext1 = "#bac2de";
    subtext0 = "#a6adc8";
    overlay2 = "#9399b2";
    overlay1 = "#7f849c";
    overlay0 = "#6c7086";
    surface2 = "#585b70";
    surface1 = "#45475a";
    surface0 = "#313244";
    base = "#1e1e2e";
    mantle = "#181825";
    crust = "#11111b";
  };
in
{
  programs.qutebrowser = {
    enable = true;
    searchEngines = {
      DEFAULT = "https://www.google.com/search?q={}";
      nix = "https://search.nixos.org/packages?channel=25.11&query={}";
    };
    keyBindings = {
      normal = {
        "<Super+l>" = "cmd-set-text :open {url}";
        "<Super+t>" = "open -t";
      };
    };
    settings = {
      url.default_page = "https://www.google.com";
      url.start_pages = "https://www.google.com";

      # Window
      window.hide_decoration = true;

      # Fonts
      fonts.default_size = config.browser.fontSize;
      fonts.web.size.minimum = 16;

      # Completion
      colors.completion.category.bg = p.base;
      colors.completion.category.border.bottom = p.mantle;
      colors.completion.category.border.top = p.overlay2;
      colors.completion.category.fg = p.green;
      colors.completion.even.bg = p.mantle;
      colors.completion.odd.bg = p.crust;
      colors.completion.fg = p.subtext0;
      colors.completion.item.selected.bg = p.surface2;
      colors.completion.item.selected.border.bottom = p.surface2;
      colors.completion.item.selected.border.top = p.surface2;
      colors.completion.item.selected.fg = p.text;
      colors.completion.item.selected.match.fg = p.rosewater;
      colors.completion.match.fg = p.text;
      colors.completion.scrollbar.bg = p.crust;
      colors.completion.scrollbar.fg = p.surface2;

      # Context menu
      colors.contextmenu.menu.bg = p.base;
      colors.contextmenu.menu.fg = p.text;
      colors.contextmenu.disabled.bg = p.mantle;
      colors.contextmenu.disabled.fg = p.overlay0;
      colors.contextmenu.selected.bg = p.overlay0;
      colors.contextmenu.selected.fg = p.rosewater;

      # Downloads
      colors.downloads.bar.bg = p.base;
      colors.downloads.error.bg = p.base;
      colors.downloads.error.fg = p.red;
      colors.downloads.start.bg = p.base;
      colors.downloads.start.fg = p.blue;
      colors.downloads.stop.bg = p.base;
      colors.downloads.stop.fg = p.green;

      # Hints
      colors.hints.bg = p.peach;
      colors.hints.fg = p.mantle;
      colors.hints.match.fg = p.subtext1;
      colors.keyhint.bg = p.mantle;
      colors.keyhint.fg = p.text;
      colors.keyhint.suffix.fg = p.subtext1;

      # Messages
      colors.messages.error.bg = p.overlay0;
      colors.messages.error.border = p.mantle;
      colors.messages.error.fg = p.red;
      colors.messages.info.bg = p.overlay0;
      colors.messages.info.border = p.mantle;
      colors.messages.info.fg = p.text;
      colors.messages.warning.bg = p.overlay0;
      colors.messages.warning.border = p.mantle;
      colors.messages.warning.fg = p.peach;

      # Prompts
      colors.prompts.bg = p.mantle;
      colors.prompts.border = p.overlay0;
      colors.prompts.fg = p.text;
      colors.prompts.selected.bg = p.surface2;
      colors.prompts.selected.fg = p.rosewater;

      # Status bar
      colors.statusbar.normal.bg = p.base;
      colors.statusbar.normal.fg = p.text;
      colors.statusbar.insert.bg = p.crust;
      colors.statusbar.insert.fg = p.rosewater;
      colors.statusbar.command.bg = p.base;
      colors.statusbar.command.fg = p.text;
      colors.statusbar.command.private.bg = p.base;
      colors.statusbar.command.private.fg = p.subtext1;
      colors.statusbar.caret.bg = p.base;
      colors.statusbar.caret.fg = p.peach;
      colors.statusbar.caret.selection.bg = p.base;
      colors.statusbar.caret.selection.fg = p.peach;
      colors.statusbar.passthrough.bg = p.base;
      colors.statusbar.passthrough.fg = p.peach;
      colors.statusbar.private.bg = p.mantle;
      colors.statusbar.private.fg = p.subtext1;
      colors.statusbar.progress.bg = p.base;
      colors.statusbar.url.fg = p.text;
      colors.statusbar.url.error.fg = p.red;
      colors.statusbar.url.hover.fg = p.sky;
      colors.statusbar.url.success.http.fg = p.teal;
      colors.statusbar.url.success.https.fg = p.green;
      colors.statusbar.url.warn.fg = p.yellow;

      # Tabs
      colors.tabs.bar.bg = p.crust;
      colors.tabs.odd.fg = p.overlay2;
      colors.tabs.odd.bg = p.surface1;
      colors.tabs.even.fg = p.overlay2;
      colors.tabs.even.bg = p.surface2;
      colors.tabs.selected.odd.fg = p.text;
      colors.tabs.selected.odd.bg = p.base;
      colors.tabs.selected.even.fg = p.text;
      colors.tabs.selected.even.bg = p.base;
      colors.tabs.indicator.error = p.red;
      colors.tabs.indicator.system = "none";

      # Tooltip
      colors.tooltip.fg = p.text;
      colors.tooltip.bg = p.mantle;

      # Webpage
      colors.webpage.preferred_color_scheme = "dark";
    };
  };
}
