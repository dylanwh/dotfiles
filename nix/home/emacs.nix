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
  emacsBase = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-nox;
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
    owner = "hlissner";
    repo = "doom-emacs";
    rev = "cc6e8400a43350be06301954320061331d0e051a";
    sha256 = "17p414n5b8nam85dyr6wyrjmshssyfbvdf49s0hh7rjaqccq1ggm";
  };
}
