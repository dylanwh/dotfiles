{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./packages.nix
    ./programs.nix
  ];

  # This is a macOS-specific setting, so it stays here.
  nixpkgs.overlays = [
    (import ./overlays/libvterm.nix)
    (import ./overlays/fish.nix)
  ];

  programs.zsh.enable = false;

  security.sudo.extraConfig = ''
    dylan ALL = (root) NOPASSWD: /run/current-system/sw/bin/nix-channel, /run/current-system/sw/bin/darwin-rebuild
  '';

  environment.shells = [
    pkgs.fish
  ];

  environment.systemPackages = with pkgs; [ iconv ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
