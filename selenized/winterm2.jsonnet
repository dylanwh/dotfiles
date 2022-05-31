local variants = import '/home/dylan/.config/selenized/variants.json';
local settings = import '/mnt/c/Users/dylan/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json';

local profile_defaults = {
  antialiasingMode: 'cleartype',
  cursorShape: 'filledBox',
  fontFace: 'Source Code Pro for Powerline',
  fontSize: 16,
  fontWeight: 'medium',
};

local rgb(s) = {
  // 0 is srgb, 1 is apple rgb
  [color]: '#' + s[color][1]
  for color in std.objectFields(s)
};

local scheme(variant, s) = {
  name: 'Selenized ' + variant,
  background: s.bg_0,
  foreground: s.fg_0,
  selectionBackground: s.fg_0,
  cursorColor: s.fg_0,
  black: s.bg_1,
  red: s.red,
  green: s.green,
  yellow: s.yellow,
  blue: s.blue,
  purple: s.magenta,
  cyan: s.cyan,
  white: s.dim_0,
  brightBlack: s.bg_2,
  brightRed: s.br_red,
  brightGreen: s.br_green,
  brightYellow: s.br_yellow,
  brightBlue: s.br_blue,
  brightPurple: s.br_magenta,
  brightCyan: s.br_cyan,
  brightWhite: s.fg_1,
};

local is_not_selenized(s) = !std.startsWith(s.name, 'Selenized ');
local non_selenized_schemes = std.filter(is_not_selenized, settings.schemes);
local selenized_schemes = [scheme(v, rgb(variants[v])) for v in std.objectFields(variants)];
local schemes = non_selenized_schemes + selenized_schemes;

local actions = [
  {
    command: {
      action: 'adjustFontSize',
      delta: 1,
    },
    keys: 'ctrl+shift+i',
  },
  {
    command: {
      action: 'adjustFontSize',
      delta: -1,
    },
    keys: 'ctrl+shift+o',
  },
  {
    command: {
      action: 'copy',
      singleLine: false,
    },
    keys: 'ctrl+shift+c',
  },
  {
    command: 'paste',
    keys: 'ctrl+shift+v',
  },
  {
    command: 'find',
    keys: 'ctrl+shift+f',
  },
  {
    command: { action: 'splitPane', split: 'auto', splitMode: 'duplicate' },
    keys: 'alt+shift+d',
  },
  {
    command: 'commandPalette',
    keys: 'ctrl+shift+p',
  },
];

local new_settings(variant) = {
  profiles: {
    defaults: profile_defaults,
    list: [
      if std.startsWith(p.name, 'Ubuntu') then
        p { colorScheme: 'Selenized ' + variant }
      else
        if p.name == 'Windows PowerShell' then
          p { colorScheme: 'Selenized dark' }
        else
          std.prune(p { colorScheme: null })
      for p in settings.profiles.list
    ],
  },
  schemes: schemes,
  actions: actions,
  old_actions: settings.actions,
  copyFormatting: false,
  copyOnSelect: false,
};

function(variant)
  std.mergePatch(settings, new_settings(variant))
