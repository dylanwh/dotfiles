{ config, pkgs, lib, ... }:

{
  imports = [
    ./packages.nix
    ./programs.nix
  ];

  # This is a macOS-specific setting, so it stays here.
  nixpkgs.overlays = [(import ./overlays/libvterm.nix)];

  programs.zsh.enable = false;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
