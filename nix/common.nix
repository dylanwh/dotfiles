{ config, pkgs, lib, ... }:

let
  commonPkgs = with pkgs; [
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
    kitty
    libvterm
    moreutils
    ncdu
    nerd-fonts.sauce-code-pro
    nmap
    nodePackages.js-beautify
    nq
    oha
    (perl.withPackages(ps: [ ps.Mojolicious ]))
    pipenv
    pv
    python313
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
  ];

  # Packages specific to macOS (darwin)
  darwinPkgs = with pkgs; [
    gawk
    gnused
    coreutils
  ];

  # Packages specific to NixOS (linux)
  linuxPkgs = with pkgs; [
    grip
    shpool
    wayland-utils
    kdePackages.discover
  ];

in
{
  programs.fish.enable = true;
  programs.tmux.enable = true;

  environment.systemPackages = commonPkgs
    ++ (lib.optionals pkgs.stdenv.isDarwin darwinPkgs)
    ++ (lib.optionals pkgs.stdenv.isLinux linuxPkgs);
}
