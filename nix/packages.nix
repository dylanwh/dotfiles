{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages =
    with pkgs;
    [
      bat
      black
      cargo
      clang-tools
      cmake
      curl
      delta
      ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
      eza
      fd
      file
      fish
      gcc
      gh
      git
      go
      gnumake
      gomodifytags
      gopls
      gore
      gotests
      gron
      html-tidy
      httpie
      hub
      imapfilter
      isort
      jq
      jsonnet
      libvterm
      moreutils
      ncdu
      nerd-fonts.sauce-code-pro
      nixfmt
      nmap
      nodePackages.js-beautify
      nq
      oha
      (perl.withPackages (ps: [ ps.Mojolicious ]))
      pipenv
      pv
      python313
      python313Packages.grip
      python313Packages.pyflakes
      python313Packages.pytest
      ripgrep
      ruby
      rustup
      shellcheck
      shfmt
      skim
      starship
      stylelint
      tmux
      vim
      vivid
      xan
    ]
    ++ (lib.optionals pkgs.stdenv.isLinux [ shpool ])
    ++ (lib.optionals pkgs.stdenv.isDarwin [
      gawk
      gnused
      coreutils-prefixed
    ]);
}
