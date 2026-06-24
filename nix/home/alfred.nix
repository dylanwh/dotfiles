{ pkgs, ... }:

let
  alfredDir = ./alfred;
  ssh-hosts = pkgs.writeShellScript "ssh-hosts" ''
    ${pkgs.perl}/bin/perl ${alfredDir}/ssh-hosts.pl | ${pkgs.jq}/bin/jq -Rn -f ${alfredDir}/ssh-hosts.jq
  '';
  ssh-launch = pkgs.writeShellScript "ssh-launch" (builtins.readFile "${alfredDir}/ssh-launch.sh");
in
{
  home.file.".local/bin/ssh-hosts".source = ssh-hosts;
  home.file.".local/bin/ssh-launch".source = ssh-launch;
}
