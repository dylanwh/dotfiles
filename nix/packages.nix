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
    ps: [
      ps.vterm
      ps.treesit-grammars.with-all-grammars
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
      claude-code-acp
      cmake
      cmatrix
      curl
      delta
      eza
      fd
      ffmpeg
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
      jujutsu
      libvterm
      moreutils
      mutt
      ncdu
      nh
      nil
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
      rage
      rclone
      ripgrep
      (rizin.withPlugins (ps: with ps; [ rz-ghidra ]))
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
      ty
      uv
      vim
      vivid
      wget
      xan
      zellij
      zstd
    ]
    ++ (lib.optionals pkgs.stdenv.isLinux [
      ((emacsPackagesFor emacs-nox).emacsWithPackages emacsPackages)
      psmisc
      shpool
      trash-cli
    ])
    ++ (lib.optionals pkgs.stdenv.isDarwin [
      coreutils-prefixed
      darwin.trash
      duti
      ((emacsPackagesFor emacs-macport).emacsWithPackages emacsPackages)
      gawk
      gnused
      graphviz-nox
      iconv
      kitty
      openssl
      qutebrowser
    ]);
}
