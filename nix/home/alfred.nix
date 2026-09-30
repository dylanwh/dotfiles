{ pkgs, ... }:

let
  ssh-hosts = pkgs.rustPlatform.buildRustPackage {
    pname = "ssh-hosts";
    version = "0.1.0";
    src = ./alfred/ssh-hosts;
    cargoLock.lockFile = ./alfred/ssh-hosts/Cargo.lock;
  };
in
{
  home.file.".local/bin/ssh-hosts".source = "${ssh-hosts}/bin/ssh-hosts";
}
