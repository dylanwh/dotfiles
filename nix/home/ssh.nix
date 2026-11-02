{
  lib,
  config,
  pkgs,
  ...
}:

let
  configDir = ../../ssh/config.d;
  files = builtins.readDir configDir;

  mkFileConfig =
    name: type:
    if type == "regular" then
      {
        name = ".ssh/config.d/${name}";
        value = {
          source = "${configDir}/${name}";
        };
      }
    else
      null;
in
{
  home.file =
    lib.pipe files [
      (lib.mapAttrsToList mkFileConfig)
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ]
    // {
      ".ssh/config".text = "Include config.d/*\n";
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      ".ssh/config.d/91-secretive".text =
        "  IdentityAgent ${config.home.homeDirectory}/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh";
    };
}
