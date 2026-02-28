{ config, ... }:

let
  c = config.selenized.srgbColors;
in
{
  programs.starship = {
    enable = true;

    settings = {
      format = builtins.concatStringsSep "" [
        "$username"
        "$hostname"
        "$shlvl"
        "$kubernetes"
        "$directory"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_status"
        "$hg_branch"
        "$docker_context"
        "$package"
        "$cmake"
        "$dart"
        "$dotnet"
        "$elixir"
        "$elm"
        "$erlang"
        "$golang"
        "$helm"
        "$java"
        "$julia"
        "$kotlin"
        "$nim"
        "$nodejs"
        "$ocaml"
        "$perl"
        "$php"
        "$purescript"
        "$python"
        "$ruby"
        "$rust"
        "$swift"
        "$terraform"
        "$zig"
        "$nix_shell"
        "$conda"
        "$memory_usage"
        "$aws"
        "$gcloud"
        "$openstack"
        "$crystal"
        "$custom"
        "$cmd_duration"
        "$fill"
        "$env_var"
        "$line_break"
        "$lua"
        "$jobs"
        "$battery"
        "$time"
        "$status"
        "$shell"
        "$character"
      ];

      git_branch = {
        symbol = " ";
        style = "fg:${c.br_violet}";
      };

      username = {
        disabled = false;
        format = "[$user]($style) at ";
      };

      hostname = {
        style = "fg:15";
        ssh_only = true;
        disabled = false;
        format = "[$hostname]($style) ";
      };

      gcloud.disabled = true;

      git_state.style = "fg:green";

      git_commit = {
        disabled = false;
        tag_disabled = false;
      };

      git_status = {
        disabled = false;
        style = "fg:${c.br_violet}";
        stashed = "S";
        deleted = "D";
      };

      directory = {
        style = "fg:12 bold";
        truncate_to_repo = true;
        truncation_length = 0;
        format = "in [$path]($style)[ $read_only]($read_only_style) ";
        read_only = "󰉐";
      };

      python = {
        symbol = " ";
        disabled = true;
      };

      perl = {
        symbol = " ";
        disabled = true;
      };

      java.disabled = true;
      aws.disabled = true;
      nodejs.disabled = true;

      shell = {
        fish_indicator = "";
        format = "[$indicator]($style)";
        bash_indicator = "bash";
        disabled = false;
      };

      lua.disabled = true;
      swift.disabled = true;

      cmake.symbol = " ";
      env_var.shpool = {
        variable = "SHPOOL_SESSION_NAME";
        style = "bg:15 fg:0 bold";
        format = "[$symbol](fg:15)[$env_value]($style)[](fg:15)";
        symbol = "";
      };

      fill.symbol = " ";
      golang.symbol = " ";
      nix_shell.symbol = " ";
    };
  };
}
