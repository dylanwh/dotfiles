{
  config,
  lib,
  pkgs,
  ...
}:

let
  nfsOptions = [
    "x-systemd.automount"
    "noauto"
    "x-systemd.idle-timeout=600"
  ];
in

{
  fileSystems."/mnt/download" = {
    device = "10.0.0.8:/volume1/Download";
    fsType = "nfs";
    options = nfsOptions;
  };

  fileSystems."/mnt/media" = {
    device = "10.0.0.8:/volume1/Media";
    fsType = "nfs";
    options = nfsOptions;
  };

  boot.supportedFilesystems = [ "nfs" ];
  services.rpcbind.enable = true;
}
