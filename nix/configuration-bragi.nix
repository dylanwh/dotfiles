{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./packages.nix
    ./system/programs.nix
    <home-manager/nix-darwin>
  ];

  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = false;

  security.sudo.extraConfig = ''
    dylan ALL = (root) NOPASSWD: /run/current-system/sw/bin/nix-channel, /run/current-system/sw/bin/darwin-rebuild, /run/current-system/sw/bin/nix-collect-garbage
  '';

  environment.shells = [
    pkgs.fish
  ];

  users.users.dylan = {
    name = "dylan";
    home = "/Users/dylan";
  };

  home-manager.users.dylan =
    { pkgs, ... }:
    {
      imports = [
        ./home/bash.nix
        ./home/emacs.nix
        ./home/fish.nix
        ./home/git.nix
        ./home/karabiner.nix
        ./home/kitty.nix
        ./home/local-bin.nix
        ./home/macos.nix
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
