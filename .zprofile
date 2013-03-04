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

source ~/.zshenv

if have keychain; then
	quiet='-q'
	if [[ -o interactive ]]; then
		quiet=''
	fi
	eval $(keychain $quiet --inherit any --eval)
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
