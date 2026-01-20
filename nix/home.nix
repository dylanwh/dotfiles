{
  config,
  pkgs,
  lib,
  ...
}:

let
  packagesModule = import ./packages.nix { inherit config pkgs lib; };
  systemPackages = packagesModule.environment.systemPackages or [ ];

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
  home.packages = deduplicatedPackages;

  home.sessionVariables = {
    LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/"; # fix the problem of dynamic link in python package
  };
}
