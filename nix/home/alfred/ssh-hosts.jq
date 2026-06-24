[
  inputs | {
    uid: .,
    title: .,
    subtitle: ("ssh to " + .),
    arg: .,
    # mods: {shift: {subtitle: ("SSH to " + .), variables: {ssh_args: ""}}}
  }
] | {items: ., cache: {seconds: 3600, loosereload: true}}
