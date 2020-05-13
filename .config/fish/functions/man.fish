function man
    env \
        LESS_TERMCAP_mb=(printf "\e[1;34m") \
        LESS_TERMCAP_md=(printf "\e[1;34m") \
        LESS_TERMCAP_me=(printf "\e[0m") \
        LESS_TERMCAP_se=(printf "\e[0m") \
        LESS_TERMCAP_so=(printf "\e[30;39;100m") \
        LESS_TERMCAP_ue=(printf "\e[0m") \
        LESS_TERMCAP_us=(printf "\e[4;36m") \
        _NROFF_U=1 \
        GROFF_NO_SGR=1 \
    man "$argv"
end
