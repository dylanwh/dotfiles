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
    /etc/nixos/hardware-configuration.nix
    ./locale.nix
    ./nfs.nix
    ./nosleep.nix
    ./packages.nix
    ./programs.nix
    ./tailscale.nix
    ./users.nix
    ./steam.nix
    ./kde.nix
  ];

  hardware.graphics.enable = true;
  hardware.nvidia.open = false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "split_lock_detect=off"
    "nvidia-drm.modeset=1"
  ];
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
  networking.networkmanager.enable = true;

  nixpkgs.config.allowUnfree = true;

  powerManagement = {
    enable = true;
    powertop.enable = true;
    cpuFreqGovernor = "schedutil"; # power, performance, ondemand
  };

  services.avahi.enable = true;

  services.sunshine = {
    enable = true;
    autoStart = true;
    capSysAdmin = true;
    openFirewall = true;
  };

  services.openssh.enable = true;

  services.udev = {
    packages = with pkgs; [
      #qmk
      qmk-udev-rules # the only relevant
      #qmk_hid
      via
      #vial
    ]; # packages
  }; # udev

  console = {
    font = "ter-powerline-v24b";
    packages = [
      pkgs.terminus_font
      pkgs.powerline-fonts
      pkgs.nerd-fonts.sauce-code-pro
    ];
    colors = [
      "252525"
      "ed4a46"
      "70b433"
      "dbb32d"
      "368aeb"
      "eb6eb7"
      "3fc5b7"
      "777777"
      "3b3b3b"
      "ff5e56"
      "83c746"
      "efc541"
      "4f9cfe"
      "ff81ca"
      "56d8c9"
      "dedede"
    ];
  };

  environment.systemPackages = with pkgs; [
    (bubblewrap.overrideAttrs (o: {
      patches = (o.patches or [ ]) ++ [ ./bwrap2.patch ];
    }))
    fbterm
    fuzzel
    kitty
    nerd-fonts.sauce-code-pro
    openrgb-with-all-plugins
    qutebrowser
    tail-tray
    via
    waybox
    wayland-utils
    wayvnc
    wlr-randr
    xwayland-satellite
  ];

  services.xserver.videoDrivers = [
    "modesetting"
    "nvidia"
  ];
  services.flatpak.enable = true;

  systemd.services.flatpak-repo = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.flatpak ];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };

  # Enable the X11 windowing system.
  # You can disable this if you're only using the Wayland session.
  # services.xserver.enable = true;

  services.displayManager.autoLogin = {
    enable = true;
    user = "dylan";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  programs._1password-gui = {
    enable = true;
    # Certain features, including CLI integration and system authentication support,
    # require enabling PolKit integration on some desktop environments (e.g. Plasma).
    polkitPolicyOwners = [ "dylan" ];
  };
  programs.firefox.enable = true;
  programs.niri.enable = true;
  programs.xwayland.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
