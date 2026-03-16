# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  pkgs,
  ...
}:

{
  imports = [
    <home-manager/nixos>
    /etc/nixos/hardware-configuration.nix
    ./packages.nix
    ./system/desktop.nix
    ./system/kde.nix
    ./system/locale.nix
    ./system/tailscale.nix
    ./system/users.nix
  ];

  hardware.nvidia.prime = {
    offload = {
      enable = true;
      enableOffloadCmd = true;
    };

    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "loki"; # Define your hostname.
  networking.networkmanager.enable = true;

  nixpkgs.config.allowUnfree = true;
  services.openssh.enable = true;

  home-manager.users.dylan =
    { pkgs, ... }:
    {
      imports = [
        ./home/alacritty.nix
        ./home/bash.nix
        ./home/emacs.nix
        ./home/fish.nix
        ./home/git.nix
        ./home/kitty.nix
        ./home/local-bin.nix
        ./home/misc.nix
        ./home/selenized.nix
        ./home/ssh.nix
        ./home/starship.nix
        ./home/tmux.nix
        ./home/vim.nix
        ./home/wezterm.nix
      ];

      desktop.host = "loki";

      home.stateVersion = "25.05";
    };

  services.keyd = {
    enable = true;
    keyboards = {
      # Give it a custom name
      default = {
        # Replace with your actual device ID from 'keyd devices'
        ids = [ "0001:0001:093d12dc" ];
        settings = {
          main = {
            # Example: remap caps lock to escape
            capslock = "leftcontrol";
            leftalt = "leftmeta";
            leftmeta = "leftalt";
            rightalt = "rightmeta";
            rightmeta = "rightalt";
          };
        };
      };
    };
  };
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
