#!/usr/bin/env python3

import json
import os
import subprocess

variants_file = os.path.expanduser("~/.config/selenized/variants.json")
with open(variants_file, "r") as f:
    variants = json.load(f)

variant_file = os.path.expanduser("~/.config/kitty/selenized-variant")

try:
    with open(variant_file, "r") as f:
        variant = f.readline().strip()
except FileNotFoundError as e:
    variant = "black"


# make it so we can access each key as a property
# e.g. colors.red instead of colors["red"]
class Color:
    def __init__(self, colors):
        self.__dict__.update(colors)


color = Color(dict((name, "#" + color[0]) for name, color in variants[variant].items()))


theme = f"""
## name: Selenized {variant}
## license: MIT
## author: Jan Warchol
## upstream: https://github.com/jan-warchol/selenized/raw/master/terminals/kitty/kitty-selenized-{variant}.conf
## blurb: Selenized {variant} color scheme for Kitty

#: The foreground and background colors
foreground {color.fg_0}
background {color.bg_0}

#: The opacity of the background. A number between 0 and 1, where 1 is
#: opaque and 0 is fully transparent.  This will only work if
#: supported by the OS (for instance, when using a compositor under
#: X11). Note that it only sets the default background color's
#: opacity. This is so that things like the status bar in vim,
#: powerline prompts, etc. still look good.  But it means that if you
#: use a color theme with a background color in your editor, it will
#: not be rendered as transparent.  Instead you should change the
#: default background color in your kitty config and not use a
#: background color in the editor color scheme. Or use the escape
#: codes to set the terminals default colors in a shell script to
#: launch your editor.  Be aware that using a value less than 1.0 is a
#: (possibly significant) performance hit.  If you want to dynamically
#: change transparency of windows set dynamic_background_opacity to
#: yes (this is off by default as it has a performance cost)
background_opacity 1

#: Allow changing of the background_opacity dynamically, using either
#: keyboard shortcuts (increase_background_opacity and
#: decrease_background_opacity) or the remote control facility.
dynamic_background_opacity no

#: How much to dim text that has the DIM/FAINT attribute set. One
#: means no dimming and zero means fully dimmed (i.e. invisible).
dim_opacity 0.625

#: The foreground for text selected with the mouse. A value of none
#: means to leave the color unchanged.
selection_foreground none

#: The background for text selected with the mouse.
selection_background {color.bg_2}

#: Tab bar colors
active_tab_foreground   {color.fg_1}
active_tab_background   {color.bg_2}
inactive_tab_foreground {color.dim_0}
inactive_tab_background {color.bg_0}
tab_bar_background      {color.bg_0}

#: The 16 terminal colors. There are 8 basic colors, each color has a
#: dull and bright version. You can also set the remaining colors from
#: the 256 color table as color16 to color255.

#: black
color0 {color.bg_1}
color8 {color.bg_2}

#: red
color1 {color.red}
color9 {color.br_red}

#: green
color2  {color.green}
color10 {color.br_green}

#: yellow
color3  {color.yellow}
color11 {color.br_yellow}

#: blue
color4  {color.blue}
color12 {color.br_blue}

#: magenta
color5  {color.magenta}
color13 {color.br_magenta}

#: cyan
color6  {color.cyan}
color14 {color.br_cyan}

#: white
color7  {color.dim_0}
color15 {color.fg_1}
"""

print(theme)
