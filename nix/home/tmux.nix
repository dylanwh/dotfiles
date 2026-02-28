{ config, pkgs, ... }:

let
  c = config.selenized.srgbColors;

  list = attrs: builtins.concatStringsSep "," attrs;
  bg = color: "bg='${color}'";
  fg = color: "fg='${color}'";
  style = attrs: content: "#[${list attrs}]${content}";

  flagStyles =
    (style [ (fg c.fg_0) (bg c.bg_2) ] "")
    + "#{?window_activity_flag,#[${fg c.yellow}],}"
    + "#{?window_silence_flag,#[${fg c.br_green}],}"
    + "#{?window_bell_flag,#[${fg c.br_cyan}],}";

  # Powerline symbols
  pl = ""; # right triangle
  pls = ""; # right thin triangle
  plr = ""; # left triangle

  themeFile = pkgs.writeText "tmux-theme.conf" ''
    # default statusbar color
    set -g status-style ${
      list [
        (bg c.fg_0)
        (fg c.bg_0)
      ]
    }

    # default window title colors
    setw -g window-status-style ${
      list [
        (bg c.orange)
        (fg "green")
      ]
    }

    # default window with an activity alert
    setw -g window-status-activity-style "none"

    # active window title colors
    setw -g window-status-current-style ""

    # pane border
    set -g pane-active-border-style ${fg c.fg_0}
    set -g pane-border-style ${fg c.dim_0}

    # message infos
    set -g message-style ${
      list [
        (bg c.fg_1)
        (fg c.bg_0)
        "bold"
      ]
    }

    # writing commands inactive
    set -g message-command-style ${
      list [
        (bg c.bg_1)
        (fg c.br_cyan)
      ]
    }

    # pane number display
    set -g display-panes-active-colour '${c.fg_1}'
    set -g display-panes-colour '${c.dim_0}'

    # clock
    setw -g clock-mode-colour '${c.cyan}'

    # bell
    setw -g window-status-bell-style ${
      list [
        (bg c.red)
        (fg c.bg_0)
      ]
    }

    ## Theme settings
    set -g status-justify "left"
    set -g status-left-style none
    set -g status-left-length "80"
    set -g status-right-style none
    set -g status-right-length "80"
    setw -g window-status-separator ""

    set -g status-left '${style [ (fg c.bg_0) (bg c.fg_0) "bold" ] " #S "}'

    set -g status-right '${
      (style [ (fg c.violet) (bg c.fg_0) "bold" "nounderscore" "noitalics" ] plr)
      + (style [ (fg c.bg_0) (bg c.violet) ] " #h ")
      + (style [ (fg c.fg_0) (bg c.violet) "bold" "noitalics" "nounderscore" ] plr)
      + (style [ (fg c.bg_2) (bg c.fg_0) ] " %H:%M ")
    }'

    setw -g window-status-current-format '${
      (style [ (fg c.fg_0) (bg c.orange) "nobold" "noitalics" "nounderscore" ] pl)
      + (style [ (fg c.bg_0) (bg c.orange) ] " #I ")
      + (style [ (fg c.bg_0) (bg c.orange) "bold" ] pls)
      + " #W#F "
      + (style [ (fg c.orange) (bg c.fg_0) "nobold" "noitalics" "nounderscore" ] pl)
    }'

    setw -g window-status-format '${
      (style [ (fg c.fg_0) (bg c.bg_2) "noitalics" ] pl)
      + flagStyles
      + " #I"
      + (style [ (fg c.fg_0) ] " ")
      + flagStyles
      + " #W#F "
      + (style [ (fg c.bg_2) (bg c.fg_0) "noitalics" ] pl)
    }'
  '';
in
{
  programs.tmux = {
    enable = true;
    prefix = "C-q";
    baseIndex = 1;
    escapeTime = 100;
    keyMode = "vi";
    mouse = true;
    terminal = "tmux-256color";

    plugins = [
      pkgs.tmuxPlugins.tmux-thumbs
    ];

    extraConfig = ''
      set -g prefix2 C-Space

      set -g lock-after-time 0
      set -g display-time 4000
      set -g status-keys vi
      set -g allow-rename off

      set -g monitor-bell off
      set -g bell-action none
      set -g activity-action other
      set -g visual-bell off
      set -g visual-silence both
      set -g allow-passthrough on

      set -ga update-environment LC_TERMINAL

      # Modern terminal features (tmux 3.2+)
      set -g terminal-features "xterm*:RGB:clipboard:ccolour:cstyle:focus:title:mouse:strikethrough:usstyle:overline"
      set -ag terminal-features "kitty*:RGB:clipboard:ccolour:cstyle:focus:title:mouse:strikethrough:usstyle:overline"

      # OSC 52 clipboard for mosh compatibility
      set -g terminal-overrides "vte*:Ms=\\E]52;c;%p2%s\\7,xterm*:Ms=\\E]52;c;%p2%s\\7"

      set -g status "on"
      set -g status-position top

      set -g set-titles on
      set -g set-titles-string "#S[#W] \"#T\""

      set -g set-clipboard on

      bind P paste-buffer -p
      bind ] paste-buffer -p
      bind v paste-buffer -p
      bind-key -T copy-mode-vi v send-keys -X begin-selection
      bind-key -T copy-mode-vi y send-keys -X copy-selection
      bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

      bind C-q send-prefix
      bind C-Space send-prefix -2

      bind Enter resize-pane -Z
      bind m setw monitor-activity
      bind M command-prompt -p "interval" "setw monitor-silence %1"
      bind / command-prompt -p "match" "setw monitor-content %1"
      bind b set status
      bind ? list-keys -T prefix

      bind D choose-client -O size -N
      bind s choose-session -O index
      bind w choose-window
      bind e new-window -n emacs et

      bind r refresh-client
      bind c new-window
      bind C new-session
      bind p previous-window
      bind n next-window
      bind C-p previous-window
      bind C-n next-window

      bind k select-pane -U
      bind j select-pane -D
      bind l select-pane -R
      bind h select-pane -L
      bind x select-pane -t :.+
      bind 0 select-window -t :10

      bind | split-window -h
      bind - split-window -v
      unbind '"'
      unbind %

      source ${themeFile}
    '';
  };
}
