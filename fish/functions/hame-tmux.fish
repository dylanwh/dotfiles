function hame-tmux
    have cargo
    and have tmux
    or return 1

    set -l tmux_dir   "$HOME/.tmux"
    set -l plugin_dir "$tmux_dir/plugins"
    mkdir -vp $plugin_dir
    hame-clone -n tmux-thumbs -u fcsonline/tmux-thumbs -p $plugin_dir/tmux-thumbs
    pushd $plugin_dir/tmux-thumbs
    test -f target/release/tmux-thumbs
    or cargo build --release
    test -f $tmux_dir
    or echo 'run-shell ~/.tmux/plugins/tmux-thumbs/tmux-thumbs.tmux' > $tmux_dir/thumbs-init.conf
    popd
end

