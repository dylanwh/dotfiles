function fish_title
    if [ -n "$SSH_CONNECTION" ]
        echo -n "["(prompt_hostname)"] "
    end
    echo fish (prompt_pwd)
end
