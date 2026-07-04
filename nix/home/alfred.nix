{ pkgs, ... }:

let
  alfredDir = ./alfred;
  ssh-hosts = pkgs.rustPlatform.buildRustPackage {
    pname = "ssh-hosts";
    version = "0.1.0";
    src = ./alfred/ssh-hosts;
    cargoLock.lockFile = ./alfred/ssh-hosts/Cargo.lock;
  };
  ssh-launch = pkgs.writeShellScript "ssh-launch" (builtins.readFile "${alfredDir}/ssh-launch.sh");
in
{
  home.file.".local/bin/ssh-hosts".source = "${ssh-hosts}/bin/ssh-hosts";
  home.file.".local/bin/ssh-launch".source = ssh-launch;
}
