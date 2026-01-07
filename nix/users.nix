{
  config,
  lib,
  pkgs,
  ...
}:

{
  users.users.dylan = {
    isNormalUser = true;
    description = "Dylan Hardison";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    shell = pkgs.fish;
    packages = with pkgs; [
      #  thunderbird
    ];
  };

  users.users.spoony = {
    isNormalUser = true;
    description = "Tina Hardison";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    shell = pkgs.fish;
    packages = with pkgs; [
      #  thunderbird
    ];
  };
  security.sudo.extraConfig = ''
    dylan ALL = (root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild
  '';
}
