function colorsave
	for color in (set -n | grep fish_color)
echo set $color $$color
end > .config/fish/colors.fish
end
