{ config, lib, pkgs, ... }:

{
  users.users.dylan = {
    isNormalUser = true;
    description = "Dylan Hardison";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    packages = with pkgs; [
    #  thunderbird
    ];
  };
}
