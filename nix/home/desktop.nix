{
  config,
  lib,
  ...
}:

{
  options.desktop.host = lib.mkOption {
    type = lib.types.str;
    description = "Host used for per-host config paths";
  };

  config =
    let
      host = config.desktop.host;
    in
    {

      home.file.".kxkbrc".source = ../../kxkbrc;

      home.file.".config/noctalia".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/per-host/${host}/noctalia";

      home.file.".config/niri".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Git/dylanwh/dotfiles/per-host/${host}/niri";
    };
}
