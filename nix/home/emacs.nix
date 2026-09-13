{
  config,
  pkgs,
  lib,
  ...
}:

let
  emacsPackages = (
    ps: [
      ps.vterm
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
  emacsBase = pkgs.emacs-nox;
  emacs = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages emacsPackages;
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
      rev = "01d68aaf6bd7db073365385cd82e1ad7e815295c";
      fetchSubmodules = true;
      hash = "sha256-+b0yNSXBTNroSuwH89Mp5wtJojaGfRgDn9G2ooorurM=";
  };
}
