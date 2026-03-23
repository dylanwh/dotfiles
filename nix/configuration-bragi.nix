{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./packages.nix
    ./system/macos.nix
    <home-manager/nix-darwin>
  ];

  home-manager.users.dylan =
    { pkgs, ... }:
    {
      imports = [
        ./home/alacritty.nix
        ./home/common.nix
        ./home/karabiner.nix
        ./home/kitty.nix
        ./home/macos.nix
        ./home/mail.nix
        ./home/qutebrowser.nix
        ./home/wezterm.nix
      ];

      # The state version is required and should stay at the version you
      # originally installed.
      home.stateVersion = "25.11";
    };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
