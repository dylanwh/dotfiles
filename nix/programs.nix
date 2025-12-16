{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.fish.enable = true;
  programs.tmux.enable = true;

  #programs._1password.enable = true;
}
