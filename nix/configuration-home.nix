{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}:

let
  packagesModule = import ./packages.nix { inherit config pkgs lib; };
  systemPackages = packagesModule.environment.systemPackages or [ ];
  config.allowUnfree = true;

  conflicts = {
    rustup = [
      "cargo"
      "rustc"
    ];
    psmisc = [ "pstree" ];
  };

  presentPackages = builtins.map (pkg: pkg.pname or "") systemPackages;

  excludedPackages = lib.flatten (
    lib.mapAttrsToList (
      keeper: conflicting: if builtins.elem keeper presentPackages then conflicting else [ ]
    ) conflicts
  );

  deduplicatedPackages = builtins.filter (
    pkg: !(builtins.elem (pkg.pname or "") excludedPackages)
  ) systemPackages;

in
{
  imports = [
    ./home/bash.nix
    ./home/emacs.nix
    ./home/fish.nix
    ./home/git.nix
    ./home/local-bin.nix
    ./home/misc.nix
    ./home/selenized.nix
    ./home/ssh.nix
    ./home/starship.nix
    ./home/tmux.nix
    ./home/vim.nix
  ];

  home.packages = deduplicatedPackages;

  home.sessionVariables = {
    LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/"; # fix the problem of dynamic link in python package
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };
}
