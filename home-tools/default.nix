{
  pkgs ? import <nixpkgs> { },
  arch ? pkgs.stdenv.hostPlatform.uname.processor,
}:
let
  isCross = arch != pkgs.stdenv.hostPlatform.uname.processor;
  staticPkgs =
    if isCross && arch == "aarch64" then
      pkgs.pkgsCross.aarch64-multiplatform-musl.pkgsStatic
    else if !isCross then
      pkgs.pkgsStatic
    else
      throw "Unsupported cross target arch: ${arch}";
  # ripgrep 15.2.0 enables tikv-jemalloc-sys on musl, but nixpkgs doesn't include rust-jemalloc-sys in buildInputs
  ripgrep = staticPkgs.ripgrep.overrideAttrs (old: {
    buildInputs = (old.buildInputs or [ ]) ++ [ staticPkgs.rust-jemalloc-sys ];
  });
  bashCfg = (import ../nix/home/bash.nix {
    config = { };
    inherit (pkgs) lib;
    inherit pkgs;
  }).programs.bash;
  selenizedVariants = builtins.fromJSON (builtins.readFile ../selenized/variants.json);
  srgbColors = pkgs.lib.mapAttrs (_: value: "#${builtins.elemAt value 1}") selenizedVariants.black;
  starshipCfg = (import ../nix/home/starship.nix {
    config = {
      selenized.srgbColors = srgbColors;
    };
  }).programs.starship;
  starshipToml = (pkgs.formats.toml { }).generate "starship.toml" starshipCfg.settings;
  bashrc = pkgs.writeText "home-tools-bashrc" ''
    # Generated from nix/home/bash.nix
    case ":$PATH:" in
      *:"$HOME/.local/bin":*) ;;
      *) export PATH="$HOME/.local/bin:$PATH" ;;
    esac

    HISTCONTROL=${pkgs.lib.escapeShellArg (builtins.concatStringsSep ":" bashCfg.historyControl)}
    HISTSIZE=${toString bashCfg.historySize}
    HISTFILESIZE=${toString bashCfg.historyFileSize}
    shopt -s ${builtins.concatStringsSep " " bashCfg.shellOptions}

    ${pkgs.lib.concatStringsSep "\n" (
      pkgs.lib.mapAttrsToList (k: v: "alias ${k}=${pkgs.lib.escapeShellArg v}") bashCfg.shellAliases
    )}

    ${bashCfg.initExtra}

    if command -v starship &>/dev/null; then
      eval "$(starship init bash)"
    fi
  '';
  tarball = pkgs.runCommand "static-tools-${arch}.tar.gz" { } ''
    mkdir -p staging/.local/bin staging/.local/share/home-tools staging/.config staging/.terminfo
    cp ${staticPkgs.rsync}/bin/rsync staging/.local/bin/
    cp ${ripgrep}/bin/rg staging/.local/bin/
    cp ${staticPkgs.fd}/bin/fd staging/.local/bin/
    ln -s fd staging/.local/bin/fdfind
    cp ${staticPkgs.jq}/bin/jq staging/.local/bin/
    cp ${staticPkgs.less}/bin/less staging/.local/bin/
    cp ${staticPkgs.starship}/bin/starship staging/.local/bin/
    cp ${bashrc} staging/.local/share/home-tools/bashrc
    cp ${starshipToml} staging/.config/starship.toml
    cp -r ${pkgs.ghostty.terminfo}/share/terminfo/. staging/.terminfo/
    chmod -R u+w staging
    tar -C staging -czf "$out" .local .config .terminfo
  '';
in
pkgs.writeShellApplication {
  name = "install-home-tools";
  text = ''
    host="''${1:?Usage: install-home-tools <host>}"
    { cat ${./remote-install.pl}; cat ${tarball}; } | ssh "$host" -- perl - "${arch}"
  '';
}
