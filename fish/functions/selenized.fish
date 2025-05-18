function selenized

    source (selenized_vars|psub)
    argparse 'v/variant=' 'm/modules=+' E/env -- $argv
    if [ -z $_flag_variant ]
        set _flag_variant black
    end
    switch $_flag_variant
        case black white dark light
            # ok
        case '*'
            echo "Invalid variant: $_flag_variant"
            return 1
    end
    if [ -z $_flag_modules ]
        set _flag_modules fish grep vim

        if have vivid
            set _flag_modules $_flag_modules vivid
        else
            set _flag_modules $_flag_modules ls_colors
        end

        have tmux
        and echo test | tmux-pp &>/dev/null
        and set _flag_modules $_flag_modules tmux

        test -n "$KITTY_PID"
        and set _flag_modules $_flag_modules kitty
    end
    set -l s_scope ''
    if [ -n $_flag_env ]
        set s_scope -gx
    end
    set -l s_colors

    for color in $selenized_colors
        set -l var selenized_{$_flag_variant}_{$color}[2]
        set $s_scope s_$color $$var
    end

    if [ $_flag_env ]
        return
    end

    set -Ux selenized_variant $_flag_variant

    for module in $_flag_modules
        switch $module
            case fish
                for var in (set -ng | grep fish_color)
                    set --erase -g $var
                end
                set -U fish_color_autosuggestion $s_dim_0 brblack
                set -U fish_color_cancel -r
                set -U fish_color_command $s_br_yellow bryellow --bold
                set -U fish_color_comment $s_dim_0 white -i --bold
                set -U fish_color_cwd $s_br_blue brblue --bold
                set -U fish_color_cwd_root $s_red red
                set -U fish_color_end $s_green green
                set -U fish_color_error $s_br_red brred
                set -U fish_color_escape $s_red red
                set -U fish_color_hg_added $s_green green
                set -U fish_color_hg_clean $s_green green
                set -U fish_color_hg_copied $s_magenta magenta
                set -U fish_color_hg_deleted $s_red red
                set -U fish_color_hg_dirty $s_red red
                set -U fish_color_hg_modified $s_yellow yellow
                set -U fish_color_hg_renamed $s_magenta magenta
                set -U fish_color_hg_unmerged $s_red red
                set -U fish_color_hg_untracked $s_yellow yellow
                set -U fish_color_history_current --bold
                set -U fish_color_host $s_fg_0 normal
                set -U fish_color_host_remote $s_yellow yellow
                set -U fish_color_match yellow --reverse
                set -U fish_color_normal $s_fg_0 normal
                set -U fish_color_operator $s_br_blue brblue
                set -U fish_color_param $s_fg_1 white
                set -U fish_color_quote $s_cyan cyan
                set -U fish_color_redirection $s_br_violet brmagenta
                set -U fish_color_search_match $s_br_yellow bryellow --background=$s_bg_2
                set -U fish_color_selection $s_fg_1 white --background=$s_bg_2
                set -U fish_color_status $s_orange red
                set -U fish_color_user $s_br_green brgreen
                set -U fish_color_valid_path $s_fg_1 white --underline
                set -U fish_color_vcs $s_violet magenta
            case iterm2
                echo -e "\033]1337;SetColors=preset=selenized-$_flag_variant\a" | tee ~/.config/fish/iterm2_colors
            case kitty
                kill -USR1 $KITTY_PID
            case ls_colors
                echo "vivid not found. LS_COLORS unchanged." >&2
                set --erase -g LS_COLORS
                set -Ux LS_COLORS (cat $HOME/.config/fish/ls_colors)
            case vivid
                set -lx VIVID_DATABASE $HOME/.config/selenized/vivid_filetypes.yml
                set -l vivid_theme (mktemp -t selenized-XXXXXX)
                command rm $vivid_theme
                begin
                    echo "colors: "
                    for color in $selenized_colors
                        set -l var s_$color
                        echo "  $color: '$$var'"
                    end
                    cat $HOME/.config/selenized/vivid.template
                    echo
                end >$vivid_theme.yml
                set --erase -g LS_COLORS
                set -Ux LS_COLORS (vivid generate $vivid_theme.yml)
                set -l s
                command rm $vivid_theme.yml
                echo $LS_COLORS | tr ':' "\n" | sort -r | tr '\n' ':' >~/.config/fish/ls_colors
            case grep
                set -Ux GREP_COLOR '7;33'
                set -Ux GREP_COLORS 'mt=7;33'
            case tmux
                set -l tmux_theme (mktemp -t selenized-XXXXXX)
                set -l selenized_env s_variant=$_flag_variant
                for color in $selenized_colors
                    set -l var selenized_{$_flag_variant}_{$color}[2]
                    set -l env s_$color
                    set $s_scope s_$color $$var
                    set -a selenized_env "$env=#$$var"
                end
                env $selenized_env tmux-pp <$HOME/.config/selenized/tmux.ep >$tmux_theme
                or continue
                if tmux info >/dev/null 2>/dev/null
                    tmux source $tmux_theme
                end
                mkdir -p ~/.tmux
                command mv $tmux_theme ~/.tmux/theme.conf
            case vim
                set -l vim_bg
                set -l vim_colorscheme
                switch $_flag_variant
                    case black
                        set vim_bg dark
                        set vim_colorscheme selenized_bw
                    case dark
                        set vim_bg dark
                        set vim_colorscheme selenized
                    case white
                        set vim_bg light
                        set vim_colorscheme selenized_bw
                    case light
                        set vim_bg light
                        set vim_colorscheme selenized
                end
                begin
                    echo "set bg=$vim_bg"
                    echo "colorscheme $vim_colorscheme"
                    echo "let g:lightline = {  'colorscheme': 'selenized_$_flag_variant' }"
                end >$HOME/.vim/theme.vim
        end
    end
end
