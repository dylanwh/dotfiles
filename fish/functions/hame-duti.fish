function hame-duti
    if have duti
        set extensions \
            pl py lua c h cpp rs js rb \
            conf toml yaml yml xml \
            css txt md rst patch \
            "text/*"
        for ext in $extensions
            duti -s net.kovidgoyal.kitty $ext editor
        end
    end
end
