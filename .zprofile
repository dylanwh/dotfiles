# Dylan William Hardison's .zlogin file.
# This shell is executed for login shells and before ~/.zshrc for interactive shells.
# See also: ~/.zshenv [~/.zprofile] ~/.zshrc ~/.zlogin ~/.zlogout

declare -gxT PERL5LIB perl5lib
declare -U path cdpath fpath manpath perl5lib # Remove dups

perl5lib=(~/lib './lib')
path=(~/bin $path)

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
