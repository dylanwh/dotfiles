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
      python313Packages.editorconfig
      python313Packages.grip
      python313Packages.pyflakes
      python313Packages.pytest
      radare2
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
      zellij
      zstd
    ]
    ++ (lib.optionals pkgs.stdenv.isLinux [
      psmisc
      shpool
      trash-cli
    ])
    ++ (lib.optionals pkgs.stdenv.isDarwin [
      coreutils-prefixed
      darwin.trash
      gawk
      gnused
      graphviz-nox
      kitty
    ]);
}
