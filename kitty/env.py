#!/usr/bin/env python3

import os
import subprocess

fish_path = []
fish_path.append("/run/current-system/sw/bin/fish")
match os.uname().sysname:
    case "Darwin":
        fish_path.append("/opt/local/bin/fish")
        fish_path.append("/usr/local/bin/fish")
        fish_path.append(os.path.expanduser("~/.local/bin/fish"))
    case "freebsd":
        fish_path.append("/usr/local/bin/fish")
    case _:
        fish_path.append("/usr/bin/fish")


fish = None
for path in fish_path:
    if os.path.exists(path):
        fish = path
        break

assert(fish is not None)

# now we use fish to find all envs
envs = subprocess.check_output([fish, "-l", "-c", "env"]).decode("utf-8")

# now we parse the output
for line in envs.splitlines():
    if line.startswith("PATH="):
        os.environ["PATH"] = line.split("=", 1)[1]
    print(f"env {line}")
