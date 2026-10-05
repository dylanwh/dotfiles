{
  config,
  lib,
  pkgs,
  ...
}:

{
  fonts.packages = with pkgs; [
    nerd-fonts.sauce-code-pro
  ];

  environment.systemPackages = with pkgs; [
    brightnessctl
    fuzzel
    i2c-tools
    liquidctl
    noctalia-shell
    openrgb-with-all-plugins
    pywalfox-native
    quickshell
    qutebrowser
    via
    waybox
    wayland-utils
    wayvnc
    wlr-randr
    xremap
    xwayland-satellite
  ];

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

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  programs.firefox.enable = true;
  programs.niri.enable = true;
  programs.xwayland.enable = true;
  #services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.plasma-login-manager.enable = true;
  services.displayManager.defaultSession = lib.mkForce "plasma";
  systemd.services.plasmalogin.environment.KWIN_FORCE_SW_CURSOR = "1";

  services.udev = {
    packages = with pkgs; [
      #qmk
      #qmk_hid
      #vial
      qmk-udev-rules # the only relevant
      via
      openrgb-with-all-plugins
    ]; # packages
  }; # udev

  services.printing.enable = true;
}
