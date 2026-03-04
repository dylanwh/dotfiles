{
  config,
  pkgs,
  lib,
  ...
}:

{
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;
  programs.fish.enable = true;
  programs.bash.enable = true;

  security.sudo.extraConfig = ''
    dylan ALL = (root) NOPASSWD: /run/current-system/sw/bin/nix-channel, /run/current-system/sw/bin/darwin-rebuild, /run/current-system/sw/bin/nix-collect-garbage
  '';

  environment.shells = [
    pkgs.fish
  ];

  environment.systemPackages = [
    pkgs.cacert
  ];

  environment.etc."ssl/certs/ca-bundle.crt".source =
    "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt";

  users.users.dylan = {
    name = "dylan";
    home = "/Users/dylan";
  };
}
