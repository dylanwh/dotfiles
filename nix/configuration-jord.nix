# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
    ./graphical.nix
    ./locale.nix
    ./nfs.nix
    ./nosleep.nix
    ./packages.nix
    ./programs.nix
    ./tailscale.nix
    ./users.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_6_17;

  boot.kernelParams = [ "split_lock_detect=off" ];

  boot.kernelPatches = [
    {
      name = "bbr";
      patch = null;
      structuredExtraConfig = with pkgs.lib.kernel; {
        TCP_CONG_BBR = yes; # enable BBR
        DEFAULT_BBR = yes; # use it by default
      };
    }
    {
      name = "bore-6.5.9";
      patch = ./0001-linux6.17.4-bore-6.5.9.patch;
    }
    {
      name = "previous-cpu-for-wakeup-v6";
      patch = ./0002-Prefer-the-previous-cpu-for-wakeup-v6.patch;
    }
  ];

  boot.kernelModules = [
    "i2c-dev"
    "i2c-piix4"
  ];
  boot.kernel.sysctl."kernel.unprivileged_userns_clone" = 1;

  networking.hostName = "jord";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile.
  # This entire list is REMOVED because it's now in common.nix
  # environment.systemPackages = with pkgs; [ ... ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  powerManagement = {
    enable = true;
    powertop.enable = true;
    cpuFreqGovernor = "schedutil"; # power, performance, ondemand
  };

  environment.systemPackages = with pkgs; [
    openrgb-with-all-plugins
    (bubblewrap.overrideAttrs (o: {
      patches = (o.patches or [ ]) ++ [
        ./bwrap2.patch
      ];
    }))
  ];

  services.hardware.openrgb = {
    enable = true;
    package = pkgs.openrgb-with-all-plugins;
    motherboard = "amd";
  };

  services.wivrn = {
    enable = true;
    openFirewall = true;

    # Write information to /etc/xdg/openxr/1/active_runtime.json, VR applications
    # will automatically read this and work with WiVRn (Note: This does not currently
    # apply for games run in Valve's Proton)
    defaultRuntime = true;

    # Run WiVRn as a systemd service on startup
    autoStart = true;

    # If you're running this with an nVidia GPU and want to use GPU Encoding (and don't otherwise have CUDA enabled system wide), you need to override the cudaSupport variable.
    package = (pkgs.wivrn.override { cudaSupport = true; });

    # You should use the default configuration (which is no configuration), as that works the best out of the box.
    # However, if you need to configure something see https://github.com/WiVRn/WiVRn/blob/master/docs/configuration.md for configuration options and https://mynixos.com/nixpkgs/option/services.wivrn.config.json for an example configuration.
    config = {
      enable = true;
      json = {
        # 1.0x foveation scaling
        scale = 1.0;
        # 100 Mb/s
        bitrate = 400000000;
        encoders = [
          {
            encoder = "nvenc";
            codec = "h265";
            # 1.0 x 1.0 scaling
            width = 1.0;
            height = 1.0;
            offset_x = 0.0;
            offset_y = 0.0;
          }
        ];
      };
    };
  };

  # services.monado = {
  #   enable = true;
  #   defaultRuntime = true; # Register as default OpenXR runtime
  # };

  # systemd.user.services.monado.environment = {
  #   STEAMVR_LH_ENABLE = "1";
  #   XRT_COMPOSITOR_COMPUTE = "1";
  #   WMR_HANDTRACKING = "0";
  # };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
