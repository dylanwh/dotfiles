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
in

{
  environment.systemPackages =
    with pkgs;
    [
      bat
      black
      clang-tools
      claude-code
      claude-agent-acp
      cmake
      cmatrix
      codex-acp
      curl
      delta
      eza
      fd
      ffmpeg
      file
      gcc
      gemini-cli
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
      kdlfmt
      kitty.terminfo
      moreutils
      ncdu
      nh
      nil
      nix-fmt
      nmap
      js-beautify
      nodejs_24
      notcurses
      nq
      oha
      opener
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
      semgrep
      shellcheck
      shfmt
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
      xan
      zellij
      zstd
    ]
    ++ (lib.optionals pkgs.stdenv.isLinux [
      psmisc
      shpool
      trash-cli
    ]);
}
