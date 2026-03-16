{
  config,
  pkgs,
  ...
}:

let
  desktopGroups = [
    "networkmanager"
    "wheel"
    "video"
    "render"
    "seat"
    "input"
    "uinput"
  ];

in
{
  programs.fish.enable = true;

  users.users.dylan = {
    isNormalUser = true;
    description = "Dylan Hardison";
    extraGroups = desktopGroups;
    shell = pkgs.fish;
    packages = [
    ];
  };

  users.users.spoony = {
    isNormalUser = true;
    description = "Tina Hardison";
    extraGroups = desktopGroups;
    shell = pkgs.fish;
    packages = [
    ];
  };
  security.sudo.extraConfig = ''
    dylan ALL = (root) NOPASSWD: /run/current-system/sw/bin/nixos-rebuild
  '';
}
