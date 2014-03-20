#!zsh
emulate zsh
declare -A MUTAGEN_LOADED

MUTAGEN_LOADED[mutagen]="autoload $0"

function mutagen_infect {
    local bundle_dir=${1:-$HOME/.zsh/bundle}
    local plugin_dir plugin_files plugin_name

    for plugin_dir in $bundle_dir/*(N/); do
        plugin_name="$(basename $plugin_dir)"
        if [[ -f $bundle_dir/$plugin_name.loader.zsh ]]; then
            plugin_files=( $bundle_dir/$plugin_name.loader.zsh )
        else
            plugin_files=($plugin_dir/(*.plugin.zsh|*.zsh)(N))
        fi

        [[ -n $MUTAGEN_LOADED[$plugin_name] ]] && continue

        if (( $#plugin_files > 0 )); then
            MUTAGEN_LOADED[$plugin_name]="source $plugin_files[1]"
            source $plugin_files[1]
        else
            MUTAGEN_LOADED[$plugin_name]="fpath $plugin_dir"
            fpath=( $plugin_dir $fpath )
        fi
    done
}
