{ lib, ... }:

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
    };
}
