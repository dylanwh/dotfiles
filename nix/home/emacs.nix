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
    rev = "5599990072f0cd2f5bd1aa63f41a9f2ffbcbd6b2";
    sha256 = "02d5krgq2hghir38c0aphllrq62ifg5fdhb5ymbxi3pjxlv2gc2j";
  };
}
