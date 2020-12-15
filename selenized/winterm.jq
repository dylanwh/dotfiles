
 "Selenized \($ENV.s_variant)" as $selenized_scheme
| "{07b52e3e-de2c-5db4-bd2d-ba144ed6c273}" as $wsl_guid
| .defaultProfile = $wsl_guid
| .schemes = [{
    name:                 $selenized_scheme,
    background:           $ENV.s_bg_0,
    foreground:           $ENV.s_fg_0,
    selectionBackground:  $ENV.s_fg_0,
    cursorColor:          $ENV.s_fg_0,
    black:                $ENV.s_bg_1,
    red:                  $ENV.s_red,
    green:                $ENV.s_green,
    yellow:               $ENV.s_yellow,
    blue:                 $ENV.s_blue,
    purple:               $ENV.s_magenta,
    cyan:                 $ENV.s_cyan,
    white:                $ENV.s_dim_0,
    brightBlack:          $ENV.s_bg_2,
    brightRed:            $ENV.s_br_red,
    brightGreen:          $ENV.s_br_green,
    brightYellow:         $ENV.s_br_yellow,
    brightBlue:           $ENV.s_br_blue,
    brightPurple:         $ENV.s_br_magenta,
    brightCyan:           $ENV.s_br_cyan,
    brightWhite:          $ENV.s_fg_1
}]
| .profiles = {
      defaults: {
        antialiasingMode:  "cleartype",
        cursorShape:       "filledBox",
        fontFace:          "Source Code Pro for Powerline",
        fontSize:          12
      },
      list: [{
          guid: "{61c54bbd-c2c6-5271-96e7-009a87ff44bf}",
          name: "Windows PowerShell",
          commandline: "powershell.exe",
          hidden: false,
          colorScheme: $selenized_scheme
        },
        {
          guid: "{0caa0dad-35be-5f56-a8ff-afceeeaa6101}",
          name: "Command Prompt",
          commandline: "cmd.exe",
          hidden: false,
          colorScheme: $selenized_scheme
        },
        {
          guid: "{b453ae62-4e3d-5e58-b989-0a998ec441b8}",
          hidden: false,
          name: "Azure Cloud Shell",
          source: "Windows.Terminal.Azure",
          colorScheme: $selenized_scheme
        },
        {
          guid: $wsl_guid,
          hidden: false,
          name: "\($ENV.WSL_DISTRO_NAME)",
          source: "Windows.Terminal.Wsl",
          colorScheme: $selenized_scheme,
          startingDirectory: "\\\\wsl$\\\($ENV.WSL_DISTRO_NAME)\\home\\dylan"
        }]
 }
| .actions = [
  {
    command: {
      action: "copy",
      singleLine: false
    },
    keys: "ctrl+shift+c"
  },
  {
    command: "paste",
    "keys": "ctrl+shift+v"
  },
  {
    command: "find",
    keys: "ctrl+shift+f"
  },
  {
    command: { "action": "splitPane", "split": "auto", "splitMode": "duplicate" },
    keys: "alt+shift+d"
  }
]
