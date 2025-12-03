{ config, pkgs, lib, ... }:

let
  # Import the packages module
  packagesModule = import ./packages.nix { inherit config pkgs lib; };
  
  # Extract system packages directly from the module's attribute
  systemPackages = packagesModule.environment.systemPackages or [];
  
  # Define conflicting package pairs (keep first, remove second)
  conflicts = {
    rustup = [ "cargo" "rustc" ];
    psmisc = [ "pstree" ];
    # Add more conflicts as needed
  };
  
  # Get list of package names present
  presentPackages = builtins.map (pkg: pkg.pname or "") systemPackages;
  
  # Determine which packages to exclude based on conflicts
  excludedPackages = lib.flatten (
    lib.mapAttrsToList (keeper: conflicting:
      if builtins.elem keeper presentPackages
      then conflicting
      else []
    ) conflicts
  );
  
  # Filter out conflicting packages
  deduplicatedPackages = builtins.filter 
    (pkg: !(builtins.elem (pkg.pname or "") excludedPackages))
    systemPackages;

in {
  home.packages = deduplicatedPackages;
}
