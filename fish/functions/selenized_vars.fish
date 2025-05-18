function selenized_vars
    set -l json_file $HOME/.config/selenized/variants.json

    set -l dump_vars '
        . as $in 
        | keys_unsorted[] 
        | . as $theme 
        | $in[$theme] 
        | keys_unsorted[] | . as $color 
        | $in[$theme][$color] 
        | [ .[] | @sh ]
        | join(" ") as $values
        | ( "selenized_\($theme)_\($color)" | @sh ) as $var
        | "\($var) \($values)"
    '

    begin
        echo -n "set -g selenized_colors "
        jq -r '.black | keys_unsorted | join(" ")' <$json_file

        echo -n "set -g selenized_variants "
        jq -r '. | keys_unsorted | join(" ")' <$json_file

        jq -r $dump_vars <$json_file | sed 's/^/set -g /'
    end

end
