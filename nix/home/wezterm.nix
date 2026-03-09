{
  config,
  lib,
  pkgs,
  ...
}:

let
  c = config.selenized.srgbColors;
  cfg = config.wezterm;

  sshDomainType = lib.types.submodule {
    options = {
      remote_address = lib.mkOption {
        type = lib.types.str;
        description = "The host:port pair of the remote server.";
      };
      username = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The username for authenticating with the remote host.";
      };
      multiplexing = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "WezTerm"
            "None"
          ]
        );
        default = null;
        description = "The type of multiplexing to use.";
      };
      assume_shell = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "Unknown"
            "Posix"
          ]
        );
        default = null;
        description = "Assumed shell dialect on the remote host.";
      };
      no_agent_auth = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Whether to disable agent auth.";
      };
      connect_automatically = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Whether to connect automatically at startup.";
      };
      timeout = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Alternative read timeout in seconds.";
      };
      remote_wezterm_path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to wezterm binary on the remote host.";
      };
      local_echo_threshold_ms = lib.mkOption {
        type = lib.types.types.int;
        default = 3000;
        description = "Round-trip latency threshold for enabling predictive local echo using";
      };
    };
  };

  # Convert attrsOf domain configs to the list-of-tables format wezterm expects
  sshDomainsList = lib.mapAttrsToList (
    name: domainCfg: lib.filterAttrs (_: v: v != null) (domainCfg // { inherit name; })
  ) cfg.sshDomains;

  sshDomainsLua = lib.generators.toLua { multiline = true; } sshDomainsList;

  sshDomainsJson = builtins.toJSON (builtins.attrNames cfg.sshDomains);
in
{
  options.wezterm.sshDomains = lib.mkOption {
    type = lib.types.attrsOf sshDomainType;
    default = { };
    description = "SSH domains for wezterm. Each key becomes the domain name.";
  };

  config.xdg.configFile."wezterm/ssh-domains.json".text = sshDomainsJson;

  config.programs.wezterm = {
    enable = true;

    colorSchemes = {
      selenized = {
        foreground = c.fg_0;
        background = c.bg_0;
        cursor_fg = c.bg_0;
        cursor_bg = c.fg_0;
        selection_fg = "none";
        selection_bg = c.bg_2;
        ansi = [
          c.bg_1
          c.red
          c.green
          c.yellow

          c.blue
          c.magenta
          c.cyan
          c.dim_0
        ];
        brights = [
          c.bg_2
          c.br_red
          c.br_green
          c.br_yellow
          c.br_blue
          c.br_magenta
          c.br_cyan
          c.fg_1
        ];
      };
    };

    extraConfig = ''
      local wezterm = require 'wezterm'
      local act     = wezterm.action
      local config  = wezterm.config_builder()

      config.font_size                    = 14
      config.font                         = wezterm.font('SauceCodePro Nerd Font Mono')
      config.color_scheme                 = 'selenized'
      config.enable_kitty_keyboard        = true
      config.disable_default_key_bindings = true
      config.window_decorations           = 'RESIZE'
      config.enable_tab_bar               = true
      config.hide_tab_bar_if_only_one_tab = true
      config.prefer_to_spawn_tabs         = false

      config.default_prog  = { '${config.home.homeDirectory}/.local/bin/with-nix-env', 'eshell' }
      config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
      config.adjust_window_size_when_changing_font_size = false

      config.ssh_domains = ${sshDomainsLua}

      config.keys = {
        { key = 'Enter',      mods = 'SUPER',       action = act.ToggleFullScreen },
        { key = '-',          mods = 'SUPER',       action = act.DecreaseFontSize },
        { key = '0',          mods = 'SUPER',       action = act.ResetFontSize },
        { key = '=',          mods = 'SUPER',       action = act.IncreaseFontSize },
        { key = 'p',          mods = 'SUPER',       action = act.ActivateCommandPalette },
        { key = 'u',          mods = 'SUPER',       action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
        { key = 'x',          mods = 'SUPER',       action = act.ActivateCopyMode },
        { key = 'c',          mods = 'SUPER',       action = act.CopyTo 'Clipboard' },
        { key = 'f',          mods = 'SUPER',       action = act.Search 'CurrentSelectionOrEmptyString' },
        { key = 'h',          mods = 'SUPER',       action = act.HideApplication },
        { key = 'k',          mods = 'SUPER',       action = act.ClearScrollback 'ScrollbackOnly' },
        { key = 'l',          mods = 'SUPER',       action = wezterm.action_callback(function(window, pane)
          local choices = {}
          local actions = {}
          local hostname = wezterm.hostname()
          for _, domain in ipairs(config.ssh_domains) do
            if domain.name == hostname then goto continue end
            local is_mux = domain.multiplexing ~= 'None'
            local color = is_mux and '${c.violet}' or '${c.orange}'
            if is_mux then
              local mux_domain = wezterm.mux.get_domain(domain.name)
              local attached = mux_domain and mux_domain:state() == 'Attached'
              local spawn_id = domain.name .. ':spawn'
              local attach_id = domain.name .. ':attach'
              local detach_id = domain.name .. ':detach'
              if attached then
                table.insert(choices, {
                  label = wezterm.format {
                    { Foreground = { Color = color } },
                    { Text = wezterm.nerdfonts.cod_server .. '  ' },
                    'ResetAttributes',
                    { Text = 'spawn ' .. domain.name },
                  },
                  id = spawn_id,
                })
                table.insert(choices, {
                  label = wezterm.format {
                    { Foreground = { Color = color } },
                    { Text = wezterm.nerdfonts.cod_debug_disconnect .. '  ' },
                    'ResetAttributes',
                    { Text = 'detach ' .. domain.name },
                  },
                  id = detach_id,
                })
              else
                table.insert(choices, {
                  label = wezterm.format {
                    { Foreground = { Color = color } },
                    { Text = wezterm.nerdfonts.cod_plug .. '  ' },
                    'ResetAttributes',
                    { Text = 'attach ' .. domain.name },
                  },
                  id = attach_id,
                })
              end
              actions[spawn_id] = function(win, p)
                win:perform_action(act.SpawnCommandInNewWindow { domain = { DomainName = domain.name } }, p)
              end
              actions[attach_id] = function(_, _)
                local d = wezterm.mux.get_domain(domain.name)
                d:attach()
              end
              actions[detach_id] = function(win, p)
                win:perform_action(act.DetachDomain { DomainName = domain.name }, p)
              end
            else
              local id = domain.name .. ':spawn'
              table.insert(choices, {
                label = wezterm.format {
                  { Foreground = { Color = color } },
                  { Text = wezterm.nerdfonts.md_console .. '  ' },
                  'ResetAttributes',
                  { Text = 'spawn ' .. domain.name },
                },
                id = id,
              })
              actions[id] = function(win, p)
                win:perform_action(act.SpawnCommandInNewWindow { domain = { DomainName = domain.name } }, p)
              end
            end
          ::continue::
          end
          table.sort(choices, function(a, b) return a.label < b.label end)
          window:perform_action(act.InputSelector {
            title = 'SSH Domains',
            fuzzy = true,
            choices = choices,
            action = wezterm.action_callback(function(win, p, id, label)
              if id and actions[id] then
                actions[id](win, p)
              end
            end),
          }, pane)
        end) },
        { key = 'm',          mods = 'SUPER',       action = act.Hide },
        { key = 'n',          mods = 'SUPER',       action = act.SpawnWindow },
        { key = 'n',          mods = 'SHIFT|SUPER', action = act.SpawnCommandInNewWindow { args = {'${config.home.homeDirectory}/.local/bin/with-nix-env', 'fish' } } },
        { key = 'q',          mods = 'SUPER',       action = act.QuitApplication },
        { key = 'r',          mods = 'SUPER',       action = act.ReloadConfiguration },
        { key = 'v',          mods = 'SUPER',       action = act.PasteFrom 'Clipboard' },
        { key = 'w',          mods = 'SUPER',       action = act.CloseCurrentPane { confirm = true } },
        { key = 'phys:Space', mods = 'SHIFT|SUPER', action = act.QuickSelect },
      }

      config.key_tables = {
        copy_mode = {
          { key = 'Tab',        mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
          { key = 'Tab',        mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
          { key = 'Enter',      mods = 'NONE',  action = act.CopyMode 'MoveToStartOfNextLine' },
          { key = 'Escape',     mods = 'NONE',  action = act.Multiple { 'ScrollToBottom', { CopyMode = 'Close' } } },
          { key = 'Space',      mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Cell' } },
          { key = '$',          mods = 'NONE',  action = act.CopyMode 'MoveToEndOfLineContent' },
          { key = '$',          mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
          { key = ',',          mods = 'NONE',  action = act.CopyMode 'JumpReverse' },
          { key = '0',          mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLine' },
          { key = ';',          mods = 'NONE',  action = act.CopyMode 'JumpAgain' },
          { key = 'F',          mods = 'NONE',  action = act.CopyMode { JumpBackward = { prev_char = false } } },
          { key = 'F',          mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = false } } },
          { key = 'G',          mods = 'NONE',  action = act.CopyMode 'MoveToScrollbackBottom' },
          { key = 'G',          mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
          { key = 'H',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportTop' },
          { key = 'H',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
          { key = 'L',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportBottom' },
          { key = 'L',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
          { key = 'M',          mods = 'NONE',  action = act.CopyMode 'MoveToViewportMiddle' },
          { key = 'M',          mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
          { key = 'O',          mods = 'NONE',  action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
          { key = 'O',          mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
          { key = 'T',          mods = 'NONE',  action = act.CopyMode { JumpBackward = { prev_char = true } } },
          { key = 'T',          mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = true } } },
          { key = 'V',          mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Line' } },
          { key = 'V',          mods = 'SHIFT', action = act.CopyMode { SetSelectionMode = 'Line' } },
          { key = '^',          mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLineContent' },
          { key = '^',          mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
          { key = 'b',          mods = 'NONE',  action = act.CopyMode 'MoveBackwardWord' },
          { key = 'b',          mods = 'ALT',   action = act.CopyMode 'MoveBackwardWord' },
          { key = 'b',          mods = 'CTRL',  action = act.CopyMode 'PageUp' },
          { key = 'c',          mods = 'CTRL',  action = act.Multiple { 'ScrollToBottom', { CopyMode = 'Close' } } },
          { key = 'd',          mods = 'CTRL',  action = act.CopyMode { MoveByPage = (0.5) } },
          { key = 'e',          mods = 'NONE',  action = act.CopyMode 'MoveForwardWordEnd' },
          { key = 'f',          mods = 'NONE',  action = act.CopyMode { JumpForward = { prev_char = false } } },
          { key = 'f',          mods = 'ALT',   action = act.CopyMode 'MoveForwardWord' },
          { key = 'f',          mods = 'CTRL',  action = act.CopyMode 'PageDown' },
          { key = 'g',          mods = 'NONE',  action = act.CopyMode 'MoveToScrollbackTop' },
          { key = 'g',          mods = 'CTRL',  action = act.Multiple { 'ScrollToBottom', { CopyMode = 'Close' } } },
          { key = 'h',          mods = 'NONE',  action = act.CopyMode 'MoveLeft' },
          { key = 'j',          mods = 'NONE',  action = act.CopyMode 'MoveDown' },
          { key = 'k',          mods = 'NONE',  action = act.CopyMode 'MoveUp' },
          { key = 'l',          mods = 'NONE',  action = act.CopyMode 'MoveRight' },
          { key = 'm',          mods = 'ALT',   action = act.CopyMode 'MoveToStartOfLineContent' },
          { key = 'o',          mods = 'NONE',  action = act.CopyMode 'MoveToSelectionOtherEnd' },
          { key = 'q',          mods = 'NONE',  action = act.Multiple { 'ScrollToBottom', { CopyMode = 'Close' } } },
          { key = 't',          mods = 'NONE',  action = act.CopyMode { JumpForward = { prev_char = true } } },
          { key = 'u',          mods = 'CTRL',  action = act.CopyMode { MoveByPage = (-0.5) } },
          { key = 'v',          mods = 'NONE',  action = act.CopyMode { SetSelectionMode = 'Cell' } },
          { key = 'v',          mods = 'CTRL',  action = act.CopyMode { SetSelectionMode = 'Block' } },
          { key = 'w',          mods = 'NONE',  action = act.CopyMode 'MoveForwardWord' },
          { key = 'y',          mods = 'NONE',  action = act.Multiple { { CopyTo = 'ClipboardAndPrimarySelection' }, { Multiple = { 'ScrollToBottom', { CopyMode = 'Close' } } } } },
          { key = 'PageUp',     mods = 'NONE',  action = act.CopyMode 'PageUp' },
          { key = 'PageDown',   mods = 'NONE',  action = act.CopyMode 'PageDown' },
          { key = 'End',        mods = 'NONE',  action = act.CopyMode 'MoveToEndOfLineContent' },
          { key = 'Home',       mods = 'NONE',  action = act.CopyMode 'MoveToStartOfLine' },
          { key = 'LeftArrow',  mods = 'NONE',  action = act.CopyMode 'MoveLeft' },
          { key = 'LeftArrow',  mods = 'ALT',   action = act.CopyMode 'MoveBackwardWord' },
          { key = 'RightArrow', mods = 'NONE',  action = act.CopyMode 'MoveRight' },
          { key = 'RightArrow', mods = 'ALT',   action = act.CopyMode 'MoveForwardWord' },
          { key = 'UpArrow',    mods = 'NONE',  action = act.CopyMode 'MoveUp' },
          { key = 'DownArrow',  mods = 'NONE',  action = act.CopyMode 'MoveDown' },
        },

        search_mode = {
          { key = 'Enter',     mods = 'NONE', action = act.CopyMode 'PriorMatch' },
          { key = 'Escape',    mods = 'NONE', action = act.CopyMode 'Close' },
          { key = 'n',         mods = 'CTRL', action = act.CopyMode 'NextMatch' },
          { key = 'p',         mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
          { key = 'r',         mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
          { key = 'u',         mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
          { key = 'PageUp',    mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
          { key = 'PageDown',  mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
          { key = 'UpArrow',   mods = 'NONE', action = act.CopyMode 'PriorMatch' },
          { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
        },
      }

      local ok, local_config = pcall(require, 'local')
      if ok and type(local_config) == 'function' then
        local_config(config)
      end

      return config
    '';
  };

  config.wezterm.sshDomains =
    let
      mux = name: {
        remote_address = name;
        assume_shell = "Posix";
      };
      ssh = name: {
        remote_address = name;
        multiplexing = "None";
        assume_shell = "Posix";
      };
    in
    {
      bragi = mux "bragi";
      frigg = mux "frigg";
      jord = mux "jord";
      odin = mux "odin";
      heimdall = ssh "heimdall";
      kvasir = ssh "kvasir";
      loki = ssh "loki";
      syn = ssh "syn";
      sjofn = ssh "sjofn";
      urd = ssh "urd";
      yggdrasil = ssh "yggdrasil";
    };
}
