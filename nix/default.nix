{ config, pkgs, lib, ... }:

# let
#   # Packages specific to macOS (darwin)
#   darwinPkgs = with pkgs; [
#     gawk
#     gnused
#     coreutils-prefixed
#   ];

  # Packages specific to NixOS (linux)

{
  imports = [
    ./packages.nix
    ./graphical.nix
  ];

  programs.fish.enable = true;
  programs.tmux.enable = true;
  programs.nano.enable = false;
}
