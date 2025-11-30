{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.fish.enable = true;
  programs.tmux.enable = true;

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    # Certain features, including CLI integration and system authentication support,
    # require enabling PolKit integration on some desktop environments (e.g. Plasma).
    polkitPolicyOwners = [ "dylan" ];
  };
}
