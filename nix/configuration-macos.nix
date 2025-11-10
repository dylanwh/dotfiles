{ config, pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  # This is a macOS-specific setting, so it stays here.
  nixpkgs.overlays = [(import ./overlays/libvterm.nix)];

  programs.zsh.enable = false;
  # programs.fish.enable = true; # <-- Removed, now in common.nix
  # programs.tmux.enable = true; # <-- Removed, now in common.nix

  # This entire list is REMOVED because it's now in common.nix
  # environment.systemPackages = with pkgs; [ ... ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
