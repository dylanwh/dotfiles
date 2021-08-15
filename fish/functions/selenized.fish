# Selenized dark

set -g selenized_colors \
    bg_0 bg_1 bg_2 dim_0 fg_0 \
    fg_1 red green yellow \
    blue magenta cyan orange \
    violet br_red br_green \
    br_yellow br_blue \
    br_magenta br_cyan \
    br_orange br_violet
set -g selenized_variants dark black light white

set -l variant dark
set -g selenized_{$variant}_bg_0 103c48 112e38
set -g selenized_{$variant}_bg_1 184956 163945
set -g selenized_{$variant}_bg_2 2d5b69 254a57
set -g selenized_{$variant}_dim_0 72898f 61777c
set -g selenized_{$variant}_fg_0 adbcbc 9faeae
set -g selenized_{$variant}_fg_1 cad8d9 bfd0d0

set -g selenized_{$variant}_red fa5750 f13c3e
set -g selenized_{$variant}_green 75b938 69ad21
set -g selenized_{$variant}_yellow dbb32d d1a416
set -g selenized_{$variant}_blue 4695f7 3a82f8
set -g selenized_{$variant}_magenta f275be e75bb3
set -g selenized_{$variant}_cyan 41c7b9 42bdaa
set -g selenized_{$variant}_orange ed8649 e26f35
set -g selenized_{$variant}_violet af88eb 9b72e9

set -g selenized_{$variant}_br_red ff665c ff4b49
set -g selenized_{$variant}_br_green 84c747 78be2e
set -g selenized_{$variant}_br_yellow ebc13d e4b424
set -g selenized_{$variant}_br_blue 58a3ff 4a91ff
set -g selenized_{$variant}_br_magenta ff84cd fb69c4
set -g selenized_{$variant}_br_cyan 53d6c7 50cfba
set -g selenized_{$variant}_br_orange fd9456 f67e41
set -g selenized_{$variant}_br_violet bd96fa ab80fc

# Selenized black

set -l variant black
set -g selenized_{$variant}_bg_0 181818 121212
set -g selenized_{$variant}_bg_1 252525 1c1c1c
set -g selenized_{$variant}_bg_2 3b3b3b 2d2d2d
set -g selenized_{$variant}_dim_0 777777 636363
set -g selenized_{$variant}_fg_0 b9b9b9 aaaaaa
set -g selenized_{$variant}_fg_1 dedede d6d6d6

set -g selenized_{$variant}_red ed4a46 e13136
set -g selenized_{$variant}_green 70b433 64a81d
set -g selenized_{$variant}_yellow dbb32d d1a416
set -g selenized_{$variant}_blue 368aeb 2d76e9
set -g selenized_{$variant}_magenta eb6eb7 de54ab
set -g selenized_{$variant}_cyan 3fc5b7 40bba8
set -g selenized_{$variant}_orange e67f43 da6930
set -g selenized_{$variant}_violet a580e2 9169dd

set -g selenized_{$variant}_br_red ff5e56 fb4343
set -g selenized_{$variant}_br_green 83c746 77bd2d
set -g selenized_{$variant}_br_yellow efc541 e9b928
set -g selenized_{$variant}_br_blue 4f9cfe 4289ff
set -g selenized_{$variant}_br_magenta ff81ca f767c0
set -g selenized_{$variant}_br_cyan 56d8c9 53d2bd
set -g selenized_{$variant}_br_orange fa9153 f37b3f
set -g selenized_{$variant}_br_violet b891f5 a67bf5

# Selenized light

set -l variant light
set -g selenized_{$variant}_bg_0 fbf3db faf0d2
set -g selenized_{$variant}_bg_1 ece3cc e7ddc0
set -g selenized_{$variant}_bg_2 d5cdb6 cbc2a6
set -g selenized_{$variant}_dim_0 909995 7e8783
set -g selenized_{$variant}_fg_0 53676d 43545a
set -g selenized_{$variant}_fg_1 3a4d53 2d3c42

set -g selenized_{$variant}_red d2212d c00221
set -g selenized_{$variant}_green 489100 3f8100
set -g selenized_{$variant}_yellow ad8900 9b7600
set -g selenized_{$variant}_blue 0072d4 005dcc
set -g selenized_{$variant}_magenta ca4898 b73088
set -g selenized_{$variant}_cyan 009c8f 038d7c
set -g selenized_{$variant}_orange c25d1e b04713
set -g selenized_{$variant}_violet 8762c6 714cbc

set -g selenized_{$variant}_br_red cc1729 b9001e
set -g selenized_{$variant}_br_green 428b00 3a7b00
set -g selenized_{$variant}_br_yellow a78300 957000
set -g selenized_{$variant}_br_blue 006dce 0059c6
set -g selenized_{$variant}_br_magenta c44392 b12b82
set -g selenized_{$variant}_br_cyan 00978a 008777
set -g selenized_{$variant}_br_orange bc5819 a9430f
set -g selenized_{$variant}_br_violet 825dc0 6b47b6

# Selenized white

set -l variant white
set -g selenized_{$variant}_bg_0 ffffff ffffff
set -g selenized_{$variant}_bg_1 ebebeb e6e6e6
set -g selenized_{$variant}_bg_2 cdcdcd c2c2c2
set -g selenized_{$variant}_dim_0 878787 747474
set -g selenized_{$variant}_fg_0 474747 373737
set -g selenized_{$variant}_fg_1 282828 1e1e1e

set -g selenized_{$variant}_red d6000c c5000d
set -g selenized_{$variant}_green 1d9700 288800
set -g selenized_{$variant}_yellow c49700 b58400
set -g selenized_{$variant}_blue 0064e4 004fe0
set -g selenized_{$variant}_magenta dd0f9d cc008e
set -g selenized_{$variant}_cyan 00ad9c 00a08a
set -g selenized_{$variant}_orange d04a00 bf3400
set -g selenized_{$variant}_violet 7f51d6 673ad0

set -g selenized_{$variant}_br_red bf0000 aa0000
set -g selenized_{$variant}_br_green 008400 147300
set -g selenized_{$variant}_br_yellow af8500 9d7100
set -g selenized_{$variant}_br_blue 0054cf 0040c8
set -g selenized_{$variant}_br_magenta c7008b b3007a
set -g selenized_{$variant}_br_cyan 009a8a 008a77
set -g selenized_{$variant}_br_orange ba3700 a62300
set -g selenized_{$variant}_br_violet 6b40c3 542bb9

function selenized
    argparse 'v/variant=' 'm/modules=+' 'E/env' -- $argv
    if [ -z $_flag_variant ]
        set _flag_variant black
    end
    if [ -z $_flag_modules ]
        set _flag_modules fish vivid grep tmux vim
        if [ -n "$WSL_DISTRO_NAME" ]
            set _flag_modules $_flag_modules winterm
        end
        if test -d /Applications/MacPorts/Alacritty.app
            set _flag_modules $_flag_modules alacritty
        end
    end
    set -l s_scope ''
    if [ -n $_flag_env ]
        set s_scope '-gx'
    end
    set -l s_colors

    for color in $selenized_colors
        set -l var selenized_{$_flag_variant}_{$color}[2]
        set $s_scope s_$color $$var
    end

    if [ $_flag_env ]
        return
    end

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
            case winterm
                set -l winterm_dir /mnt/c/Users/dylan/AppData/Local/Packages/Microsoft.WindowsTerminal_*/LocalState
                set -l s_dir   $HOME/.config/selenized
                set -l s_file  $winterm_dir/settings.json
                set -l jsonnet_args -A "WSL_DISTRO_NAME=$WSL_DISTRO_NAME" -A "s_variant=$_flag_variant"
                for color in $selenized_colors
                    set -l var selenized_{$_flag_variant}_{$color}[2]
                    set -l name s_$color
                    set -a jsonnet_args -A "$name=#$$var"
                end
                jsonnet $jsonnet_args $s_dir/winterm.jsonnet > $s_file.new
                command mv -f $s_file.new $s_file
            case alacritty
                set -l s_dir   $HOME/.config/selenized
                set -l s_file  $HOME/.config/alacritty/alacritty.yml
                set -l jsonnet_args -A SHELL=$SHELL -A "s_variant=$_flag_variant"
                for color in $selenized_colors
                    set -l var selenized_{$_flag_variant}_{$color}[2]
                    set -l name s_$color
                    set -a jsonnet_args -A "$name=$$var"
                end
                mkdir -p (dirname $s_file)
                jsonnet -y $jsonnet_args $s_dir/alacritty.jsonnet > $s_file
            case vivid
                if not have vivid
                    echo "vivid not found; cargo install vivid?" >&2
                    set --erase -g LS_COLORS
                    set -Ux LS_COLORS (cat ~/.config/fish/ls_colors)
                    continue
                end
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
                echo $LS_COLORS | tr ':' "\n" | sort -r | tr '\n' ':'  > ~/.config/fish/ls_colors
            case grep
                set -Ux GREP_COLOR '7;33'
            case tmux
                set -l tmux_theme (mktemp -t selenized-XXXXXX)
                set -l selenized_env s_variant=$_flag_variant
                for color in $selenized_colors
                    set -l var selenized_{$_flag_variant}_{$color}[2]
                    set -l env s_$color
                    set $s_scope s_$color $$var
                    set -a selenized_env "$env=#$$var"
                end
                env $selenized_env tmux-pp < $HOME/.config/selenized/tmux.ep >$tmux_theme
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
