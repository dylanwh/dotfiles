{
  ...
}:

{
  programs.qutebrowser = {
    enable = true;
    keyBindings = {
      normal = {
        "<Super+l>" = "cmd-set-text :open {url}";
      };
    };
  };
}
