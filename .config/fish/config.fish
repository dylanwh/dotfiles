apply-fish-defaults

if have plenv
    # commented out because it's slow
    # source (plenv init - fish | grep -v 'set -gx PATH' | psub)
end

if have pyenv
    # commented out because it's slow
    # source (pyenv init - --no-rehash fish | grep -v 'set -gx PATH' | psub)
end

if [ -x ~/.linuxbrew/bin/brew ]
    # commented out because it's slow
    # source (~/.linuxbrew/bin/brew shellenv | grep -v fish_user_paths | psub)
end

if have chef
    source (chef shell-init fish | grep -v 'set -gx PATH' | psub)
    path add  "/opt/chefdk/bin" "/Users/dylan/.chefdk/gem/ruby/2.5.0/bin" "/opt/chefdk/embedded/bin"
end

if status --is-interactive
    set -U BASE16_SHELL "$HOME/.config/base16-shell"
    if test -d $BASE16_SHELL
        # commented out because it's slow
        # source $BASE16_SHELL/profile_helper.fish
        # load currently active theme...
    end

    if test -e ~/.base16_theme
        set -l SCRIPT_NAME (basename (realpath ~/.base16_theme) .sh)
        set -gx BASE16_THEME (string match 'base16-*' $BASE16_THEME  | string sub -s (string length 'base16-*'))
        eval sh '"'(realpath ~/.base16_theme)'"'
    end
end
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
