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
    "$HOME/.local/bin"
    "$HOME/.emacs.d/bin"
  ];
  home.packages = [ emacs ];
  home.file.".doom.d".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/doom";
  home.file.".emacs.d".source = pkgs.fetchFromGitHub {
    owner = "hlissner";
    repo = "doom-emacs";
    rev = "38d94da67dc84897a4318714dcc48494c016d8c4";
    sha256 = "Uc6qONH3jjUVDgW+pPBCGC7mh88ZY05u1y37fQrsxq0=";
  };
}
