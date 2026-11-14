{
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
  nix-fmt = (
    if lib.versionOlder "25.11" pkgs.lib.version then pkgs.nixfmt else pkgs.nixfmt-rfc-style
  );
  ghostty = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
  oha = pkgs.oha.overrideAttrs (old: {
    buildInputs = (old.buildInputs or [ ]) ++ [
      pkgs.aws-lc
      pkgs.sqlite
      pkgs.zstd
    ];
  });
  pi-acp = pkgs.callPackage ./packages/pi-acp.nix { };
in

{
  environment.systemPackages =
    with pkgs;
    [
      antigravity-acp
      antigravity-cli
      bat
      black
      clang-tools
      claude-agent-acp
      claude-code
      cmake
      cmatrix
      curl
      delta
      dig
      eza
      fd
      ffmpeg
      file
      gcc
      gh
      ghostty.terminfo
      git
      github-copilot-cli
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
      js-beautify
      jsonnet
      jujutsu
      kdlfmt
      kitty.terminfo
      moreutils
      ncdu
      nh
      nil
      nix-fmt
      nmap
      nodejs_24
      notcurses
      nq
      oha
      pandoc
      (perl.withPackages perlModules)
      pi-acp
      pi-coding-agent
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
      restic
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
      tdf
      tmux
      tree
      ty
      uv
      vim
      vivid
      wget
      zellij
      zstd
    ]
    ++ (lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      psmisc
      rbw
      trash-cli
    ]);
}
