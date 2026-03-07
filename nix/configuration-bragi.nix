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
        ./home/bash.nix
        ./home/emacs.nix
        ./home/fish.nix
        ./home/git.nix
        ./home/karabiner.nix
        ./home/kitty.nix
        ./home/local-bin.nix
        ./home/macos.nix
        ./home/mail.nix
        ./home/misc.nix
        ./home/selenized.nix
        ./home/ssh.nix
        ./home/starship.nix
        ./home/tmux.nix
        ./home/vim.nix
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
