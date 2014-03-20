# Dylan William Hardison's .zprofile file.
# This script is executed for login shells and before ~/.zshrc for interactive shells.
# See also: ~/.zshenv [~/.zprofile] ~/.zshrc ~/.zlogin ~/.zlogout
# #'

case $HOST in
    lofn)
        # to enable UTF-8.
        export LANG=en_US.UTF-8
    ;; 
esac

if [[ -f /etc/arch-release && -d /etc/profile.d ]]; then
    for file in /etc/profile.d/*.(z|)sh(N); do
        source $file
        unset file
    done
fi

setopt globalrcs

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh/lib,~/:
