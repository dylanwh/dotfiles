#!/bin/sh

set -eu

src_dir="$(dirname "$(realpath "$0")")"

hostname="$(hostname -s)"

case "$(uname -s)" in
"Darwin")
    nix_config="$src_dir/configuration-$hostname.nix"
    nix_dir="/etc/nix-darwin"
    nix_rebuild="darwin-rebuild"
    if ! [ -f "$nix_config" ]; then
        echo "$nix_config does not exist" 1>&2
        exit 1
    fi
    ;;
"Linux")
    if [ -d /etc/nixos ]; then
        nix_dir="/etc/nixos"
        nix_config="$src_dir/configuration-$hostname.nix"
        nix_rebuild="nixos-rebuild"
        if ! [ -f "$nix_config" ]; then
            echo "$nix_config does not exist" 1>&2
            exit 1
        fi
    else
        echo "not sure what this is" 1>&1
        exit 1
    fi
    ;;
esac

tempfile="$(mktemp)"
echo "{ config, pkgs, lib, ... }: { imports = [ $nix_config ]; }" >"$tempfile"
trap 'rm -f $tempfile' EXIT

if ! cmp $tempfile "$nix_dir/configuration.nix"; then
    sudo tee "$nix_dir/configuration.nix" <$tempfile >/dev/null
fi

set -x
sudo "$nix_rebuild" "$@"
