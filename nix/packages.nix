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
  emacsPackages = (
    epkgs: [
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
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
      ((emacsPackagesFor emacs).emacsWithPackages emacsPackages)
      eza
      fd
      file
      fish
      gcc
      gh
      git
      glslang
      gnumake
      go
      gomodifytags
      gopls
      gore
      gotests
      gron
      html-tidy
      htop
      httpie
      hub
      hyperfine
      iconv
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
      nodejs_24
      notcurses
      nq
      oha
      pandoc
      (perl.withPackages perlModules)
      pipenv
      pstree
      pv
      python313
      python313Packages.grip
      python313Packages.pyflakes
      python313Packages.pytest
      rage
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
      tree
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
      kitty
    ]);
}
