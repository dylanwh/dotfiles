{
  config,
  ...
}:

let
  c = config.selenized.srgbColors;
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

    };
  };
}
