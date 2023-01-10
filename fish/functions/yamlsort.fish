function yamlsort
    yq 'sort_keys(..)' $argv[1] | sponge $argv[1]
end
