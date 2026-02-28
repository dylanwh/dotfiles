{ lib, ... }:

let
  binDir = ../../bin;
  # Read the directory: returns an attrSet where names are filenames
  # and values are types ("regular", "directory", "symlink")
  files = builtins.readDir binDir;

  # Filter for regular files and format them for home.file
  # This creates: { ".local/bin/name" = { source = ../bin/name; }; }
  mkFileConfig =
    name: type:
    if type == "regular" then
      {
        name = ".local/bin/${name}";
        value = {
          source = "${binDir}/${name}";
        };
      }
    else
      null;

in
{
  # mapAttrs' transforms the list, and filterAttrs removes anything that wasn't a file
  home.file = lib.pipe files [
    (lib.mapAttrsToList mkFileConfig)
    (builtins.filter (x: x != null))
    builtins.listToAttrs
  ];
}
