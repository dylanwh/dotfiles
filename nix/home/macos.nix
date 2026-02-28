{
  config,
  pkgs,
  lib,
  ...
}:

{
  targets.darwin.defaults = {
    "com.apple.screencapture" = {
      location = "~/Documents/Screenshots";
    };

    "com.apple.dock" = {
      autohide = true;
      autohide-delay = 0.1;
      autohide-time-modifier = 0.15;
      expose-group-apps = true;
      largesize = 128;
      mru-spaces = false;
      show-recents = false;
      showAppExposeGestureEnabled = true;
      tilesize = 67;
      wvous-br-corner = 14;
    };

    NSGlobalDomain = {
      AppleAccentColor = 4;
      AppleActionOnDoubleClick = "Fill";
      AppleEnableSwipeNavigateWithScrolls = false;
      AppleFirstWeekday = {
        gregorian = 2;
      };
      AppleICUDateFormatStrings = {
        "1" = "y-MM-dd";
      };
      AppleICUForce24HourTime = true;
      AppleInterfaceStyle = "Dark";
      AppleKeyboardUIMode = 1;
      ApplePressAndHoldEnabled = false;
      AppleReduceDesktopTinting = true;
      AppleScrollerPagingBehavior = 1;
      AppleShowAllExtensions = true;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSCloseAlwaysConfirmsChanges = true;
      NSQuitAlwaysKeepsWindows = true;
      WebAutomaticSpellingCorrectionEnabled = false;
      "com.apple.keyboard.fnState" = true;
      "com.apple.sound.beep.sound" = "/System/Library/Sounds/Tink.aiff";
      "com.apple.springing.enabled" = false;
      "com.apple.swipescrolldirection" = true;
    };

    "com.apple.WindowManager" = {
      EnableTiledWindowMargins = false;
      GloballyEnabled = false;
    };

    "com.apple.menuextra.clock" = {
      Show24Hour = true;
    };

    "com.apple.desktopservices" = {
      DSDontWriteNetworkStores = true;
    };

    "com.microsoft.VSCode" = {
      ApplePressAndHoldEnabled = false;
    };

    "com.valvesoftware.steamlink" = {
      ApplePressAndHoldEnabled = false;
    };

    "com.googlecode.iterm2" = {
      PrefsCustomFolder = "~/.config/iterm2";
      LoadPrefsFromCustomFolder = true;
    };
  };

  home.packages = with pkgs; [
    coreutils-prefixed
    darwin.trash
    duti
    gawk
    gnused
    graphviz-nox
    iconv
    openssl
    qutebrowser
  ];

  home.activation.duti = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    extensions=(pl py lua c h cpp rs js rb conf toml yaml yml xml css txt md rst patch "text/*")
    for ext in "''${extensions[@]}"; do
      ${pkgs.duti}/bin/duti -s net.kovidgoyal.kitty "$ext" editor
    done
  '';

  launchd.agents.opener = {
    enable = true;
    config = {
      ProgramArguments = [ "${pkgs.opener}/bin/opener" ];
      RunAtLoad = true;
      KeepAlive = true;
    };
  };

  targets.darwin.currentHostDefaults = {
    NSGlobalDomain = {
      NSStatusItemSelectionPadding = 6;
      NSStatusItemSpacing = 6;
    };
  };
}
