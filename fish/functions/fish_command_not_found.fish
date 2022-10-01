function fish_command_not_found
    if [ -x /usr/libexec/command-not-found ]
        /usr/libexec/command-not-found $argv[1]
    else
        __fish_default_command_not_found_handler $argv
    end
end
