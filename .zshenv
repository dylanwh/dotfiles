# Dylan William Hardison's .zshenv file. #'
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

emulate zsh

declare -gxT PERL5LIB perl5lib # declare array
declare -U path perl5lib       # remove duplicates

have() {
    whence -p "$@" &>/dev/null || return 1
}

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh/lib,~/:
