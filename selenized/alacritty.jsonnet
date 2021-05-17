local color(n) = '0x' + n;
local font_face = 'Source Code Pro for Powerline';
local font_size = 18;
local font = {
  normal: { family: font_face, style: 'Regular' },
  bold: { family: font_face, style: 'Bold' },
  italic: { family: font_face, style: 'Italic' },
  size: font_size,
  use_thin_strokes: true,
};


local tmux(s='') = '\u0011' + s;
local tabs = [
  { key: 'Key' + n, mods: 'Command', chars: tmux(n) }
  for n in [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
];
local key_bindings = [
  { key: 'Q', mods: 'Control', chars: tmux() },
  { key: 'T', mods: 'Command', chars: tmux('c') },
  { key: 'W', mods: 'Command', chars: tmux('&') },
] + tabs;

// \u00391
function(SHELL,
         s_variant,
         s_bg_0,
         s_bg_1,
         s_bg_2,
         s_dim_0,
         s_fg_0,
         s_fg_1,
         s_red,
         s_green,
         s_yellow,
         s_blue,
         s_magenta,
         s_cyan,
         s_orange,
         s_violet,
         s_br_red,
         s_br_green,
         s_br_yellow,
         s_br_blue,
         s_br_magenta,
         s_br_cyan,
         s_br_orange,
         s_br_violet)
  [
    {
      colors: {
        primary: {
          background: color(s_bg_0),
          foreground: color(s_fg_0),
        },
        normal: {
          black: color(s_bg_1),
          s_red: color(s_red),
          s_green: color(s_green),
          s_yellow: color(s_yellow),
          s_blue: color(s_blue),
          s_magenta: color(s_magenta),
          s_cyan: color(s_cyan),
          white: color(s_dim_0),
        },
        bright: {
          black: color(s_bg_2),
          s_red: color(s_br_red),
          s_green: color(s_br_green),
          s_yellow: color(s_br_yellow),
          s_blue: color(s_br_blue),
          s_magenta: color(s_br_magenta),
          s_cyan: color(s_br_cyan),
          white: color(s_fg_1),
        },
      },
      font: font,
      key_bindings: key_bindings,
      bell: { animation: 'EaseOutCirc', duration: 100 },
      shell: {
        program: SHELL,
        args: ['-l'],
      },
    },
  ]
