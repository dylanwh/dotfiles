{
  config,
  pkgs,
  lib,
  ...
}:

let
  emacsPackages = (
    ps: [
      ps.mu4e
      (ps.treesit-grammars.with-grammars (
        g:
        builtins.attrValues (
          builtins.removeAttrs g [
            "tree-sitter-quint"
          ]
        )
      ))
    ]
  );
  # emacsBase = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-macport else pkgs.emacs-nox;
  emacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages emacsPackages;
in
{
  home.sessionVariables.DOOMLOCALDIR = "$HOME/.local/doom";
  home.sessionVariables.EDITOR = "$HOME/.local/bin/emacsedit";
  home.sessionVariables.VISUAL = "$HOME/.local/bin/emacsedit";
  home.sessionPath = [
    "$HOME/.emacs.d/bin"
  ];
  home.packages = [
    emacs
    pkgs.emacs-lsp-booster
  ];
  home.file.".doom.d".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/doom";
  home.file.".emacs.d".source = pkgs.fetchFromGitHub {
    owner = "doomemacs";
    repo = "core";
    rev = "59cdaa32ae933469bb6a1fb3cadee8a988c15968";
    fetchSubmodules = true;
    hash = "sha256-Wu2Tztg+QibAewCBeI3UKxN2mheSU2aoXEtb19RuIPM=";
  };
}
