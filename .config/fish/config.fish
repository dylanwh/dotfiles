set GNU_PATH /usr/local/opt/*/libexec/gnubin

set -l config_version (stat -c %Y ~/.config/fish/functions/apply-fish-defaults.fish)

test -z "$dylan_config_version"
or test "$config_version" -gt "$dylan_config_version"
and apply-fish-defaults

set -U dylan_config_version $config_version


for env_file in plenv pyenv chef
    test -f ~/.config/fish/{$env_file}.fish
    and source ~/.config/fish/{$env_file}.fish
end

if status --is-interactive
    set -U BASE16_SHELL "$HOME/.config/base16-shell"
    if test -d $BASE16_SHELL
        function base16-load
            source $BASE16_SHELL/profile_helper.fish
        end
        # load currently active theme...
    end

    if test -e ~/.base16_theme
        set -l SCRIPT_NAME (basename (realpath ~/.base16_theme) .sh)
        set -gx BASE16_THEME (string match 'base16-*' $BASE16_THEME  | string sub -s (string length 'base16-*'))
        eval sh '"'(realpath ~/.base16_theme)'"'
    end
end

test -f {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
