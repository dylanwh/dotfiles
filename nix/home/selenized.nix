{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.selenized;

  variants = builtins.fromJSON (builtins.readFile ../../selenized/variants.json);
  colors = variants.${cfg.variant};

  colorsYaml = lib.concatStringsSep "\n" (
    [ "colors:" ] ++ lib.mapAttrsToList (name: value: "  ${name}: '${builtins.elemAt value 0}'") colors
  );

  themeYaml = pkgs.writeText "selenized-vivid-theme.yml" ''
    ${colorsYaml}
    ${builtins.readFile ../../selenized/vivid.template}
  '';

  filetypesDb = ../../selenized/vivid_filetypes.yml;

  lsColors = pkgs.runCommand "selenized-ls-colors" { nativeBuildInputs = [ pkgs.vivid ]; } ''
    VIVID_DATABASE=${filetypesDb} vivid generate ${themeYaml} > $out
  '';
in
{
  options.selenized = {
    variant = lib.mkOption {
      type = lib.types.enum [
        "black"
        "dark"
        "light"
        "white"
      ];
      default = "black";
      description = "Selenized color variant";
    };

    vimBg = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = if cfg.variant == "light" || cfg.variant == "white" then "light" else "dark";
      description = "Vim background value derived from selenized variant";
    };

    vimColorscheme = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = if cfg.variant == "dark" || cfg.variant == "light" then "selenized" else "selenized_bw";
      description = "Vim colorscheme derived from selenized variant";
    };

    colors = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = lib.mapAttrs (_: value: "#${builtins.elemAt value 0}") colors;
      description = "Selenized Apple P3 hex colors for the active variant";
    };

    srgbColors = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = lib.mapAttrs (_: value: "#${builtins.elemAt value 1}") colors;
      description = "Selenized sRGB hex colors for the active variant";
    };
  };

  config = {
    home.sessionVariables = {
      LS_COLORS = builtins.readFile lsColors;
      GREP_COLOR = "7;33";
      GREP_COLORS = "mt=7;33";
    };
  };
}
