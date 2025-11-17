{
  config,
  lib,
  pkgs,
  ...
}:

let
  perlModules = (
    ps: [
      ps.Mojolicious
      ps.CpanelJSONXS
    ]
  );
in

{
  environment.systemPackages =
    with pkgs;
    [
      bat
      black
      cargo
      clang-tools
      cmake
      cmatrix
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
      gnumake
      go
      gomodifytags
      gopls
      gore
      gotests
      gron
      html-tidy
      httpie
      hub
      imapfilter
      iperf
      isort
      jq
      jsonnet
      libvterm
      moreutils
      ncdu
      nerd-fonts.sauce-code-pro
      nixfmt-rfc-style
      nmap
      nodePackages.js-beautify
      notcurses
      nq
      oha
      (perl.withPackages perlModules)
      pipenv
      pv
      python313
      python313Packages.grip
      python313Packages.pyflakes
      python313Packages.pytest
      rclone
      ripgrep
      rsync
      ruby
      ruff
      rustup
      shellcheck
      shfmt
      skim
      socat
      starship
      stylelint
      tmux
      uv
      vim
      vivid
      xan
      zstd
    ]
    ++ (lib.optionals pkgs.stdenv.isLinux [
      psmisc
      shpool
      trash-cli
    ])
    ++ (lib.optionals pkgs.stdenv.isDarwin [
      gawk
      gnused
      coreutils-prefixed
      darwin.trash
    ]);
}
