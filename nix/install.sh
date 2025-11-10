#!/bin/sh

src_dir="$(dirname "$(realpath "$0")")"

if [ -d /etc/nixos ]; then
    nix_dir=/etc/nixos
    suffix="nixos"
elif [ -d /etc/nix-darwin ]; then
    nix_dir=/etc/nix-darwin
    suffix="macos"
else
    echo "not sure where nix is" 2>&1
    exit 1
fi

sudo mkdir -vp $nix_dir/overlays
for file in "$src_dir/overlays"/*.nix; do
    sudo ln -svf "$file" "$nix_dir/overlays/$(basename $file)"
done

sudo ln -svf "$src_dir/common.nix" "$nix_dir/common.nix"
sudo ln -svf "$src_dir/configuration-$suffix.nix" "$nix_dir/configuration.nix"
