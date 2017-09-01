function initfish
	colorload
abbrload
set -U fish_user_paths $HOME/bin $HOME/.plenv/shims $HOME/.cargo/bin $HOME/.gem/ruby/2.0.0/bin
if set -q $RUST_SRC_PATH
  have rustc
  and set -l rust_sysroot (rustc --print sysroot)
  test -n $rust_sysroot
  and test -d "$rust_sysroot/lib/rustlib/src/rust/src"
  and set -xU RUST_SRC_PATH "$rust_sysroot/lib/rustlib/src/rust/src"
end
end
