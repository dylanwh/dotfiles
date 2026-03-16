{
  config,
  lib,
  pkgs,
  ...
}:

let
  tuigreet = "${pkgs.tuigreet}/bin/tuigreet";
  niri-session = "${pkgs.niri}/share/wayland-sessions";
in

{

  fonts.packages = with pkgs; [
    nerd-fonts.sauce-code-pro
  ];

  environment.systemPackages = with pkgs; [
    brightnessctl
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

  programs._1password-gui = {
    enable = true;
    # Certain features, including CLI integration and system authentication support,
    # require enabling PolKit integration on some desktop environments (e.g. Plasma).
    polkitPolicyOwners = [ "dylan" ];
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${tuigreet} --time --remember --remember-user-session --sessions ${niri-session},startplasma-wayland";
        user = "greeter";
      };
    };
  };

  # this is a life saver.
  # literally no documentation about this anywhere.
  # might be good to write about this...
  # https://www.reddit.com/r/NixOS/comments/u0cdpi/tuigreet_with_xmonad_how/
  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal"; # Without this errors will spam on screen
    # Without these bootlogs will spam on screen
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };

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
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
    config.niri.default = [
      "gnome"
      "gtk"
    ];
  };

  home-manager.users.dylan =
    { pkgs, config, ... }:
    {
      imports = [
        ../home/desktop.nix
        ../home/qutebrowser.nix
      ];

      home.stateVersion = "25.05";

    };

}
