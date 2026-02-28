{
  config,
  lib,
  pkgs,
  ...
}:

let
  functionsDir = ../../fish/functions;
  s = lib.mapAttrs (_: v: lib.removePrefix "#" v) config.selenized.srgbColors;

  # Read the directory to get an attrSet of filenames
  files = builtins.readDir functionsDir;

  # Create the target mapping for Home Manager
  # Only processes regular files (ignoring subdirs or hidden files)
  mkFishConfig =
    name: type:
    if type == "regular" then
      {
        name = ".config/fish/functions/${name}";
        value = {
          source = "${functionsDir}/${name}";
        };
      }
    else
      null;

in
{
  programs.fish.enable = true;
  programs.fish.shellInit = ''
    set -x REALNAME "${config.programs.git.settings.user.name}"
    set -x EMAIL "${config.programs.git.settings.user.email}"
    set -x MANPAGER 'less -s'
    set -x LANG en_US.UTF-8
    set -x LC_COLLATE POSIX # sort in POSIX order.
    set -x LC_TIME POSIX
    set -x TZ America/Los_Angeles
    set -g OS (uname)
    set --erase CDPATH
    set -g fish_greeting ""


    test -f $HOME/.config/fish/local.fish
    and source $HOME/.config/fish/local.fish

    # Selenized fish colors
    set -g fish_color_autosuggestion ${s.dim_0} brblack
    set -g fish_color_cancel -r
    set -g fish_color_command ${s.br_yellow} bryellow --bold
    set -g fish_color_comment ${s.dim_0} white -i --bold
    set -g fish_color_cwd ${s.br_blue} brblue --bold
    set -g fish_color_cwd_root ${s.red} red
    set -g fish_color_end ${s.green} green
    set -g fish_color_error ${s.br_red} brred
    set -g fish_color_escape ${s.red} red
    set -g fish_color_hg_added ${s.green} green
    set -g fish_color_hg_clean ${s.green} green
    set -g fish_color_hg_copied ${s.magenta} magenta
    set -g fish_color_hg_deleted ${s.red} red
    set -g fish_color_hg_dirty ${s.red} red
    set -g fish_color_hg_modified ${s.yellow} yellow
    set -g fish_color_hg_renamed ${s.magenta} magenta
    set -g fish_color_hg_unmerged ${s.red} red
    set -g fish_color_hg_untracked ${s.yellow} yellow
    set -g fish_color_history_current --bold
    set -g fish_color_host ${s.fg_0} normal
    set -g fish_color_host_remote ${s.yellow} yellow
    set -g fish_color_match yellow --reverse
    set -g fish_color_normal ${s.fg_0} normal
    set -g fish_color_operator ${s.br_blue} brblue
    set -g fish_color_param ${s.fg_1} white
    set -g fish_color_quote ${s.cyan} cyan
    set -g fish_color_redirection ${s.br_violet} brmagenta
    set -g fish_color_search_match ${s.br_yellow} bryellow --background=${s.bg_2}
    set -g fish_color_selection ${s.fg_1} white --background=${s.bg_2}
    set -g fish_color_status ${s.orange} red
    set -g fish_color_user ${s.br_green} brgreen
    set -g fish_color_valid_path ${s.fg_1} white --underline
    set -g fish_color_vcs ${s.violet} magenta
    set -g fish_pager_color_description yellow --italics
    set -g fish_pager_color_prefix ${s.fg_0} --bold --underline
    set -g fish_pager_color_progress ${s.fg_0} --background=${s.cyan} --bold
    set -g fish_pager_color_selected_background -r
  '';
  programs.fish.shellAliases = {
    emacs = "emacsedit";
  };

  # Generate the home.file set dynamically
  home.file =
    lib.pipe files [
      (lib.mapAttrsToList mkFishConfig)
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ]
    // {
      ".config/fish/conf.d/abbr.fish".source = ../../fish/abbr.fish;
    };

}
