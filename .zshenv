# Dylan William Hardison's .zshenv file. #'
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

emulate zsh

declare -gxT PERL5LIB perl5lib # declare array
declare -U path perl5lib       # remove duplicates

have() {
    whence -p "$@" &>/dev/null || return 1
}

if [[ -o rcs ]]; then
    export REALNAME="Dylan William Hardison"
    export EMAIL="dylan@hardison.net"
    export EDITOR="vim"
    export VISUAL="$EDITOR"
    export BROWSER="firefox"
    export MANPAGER='less -s'

    export HOST="${HOST/.*/}"
    export OSTYPE="$OSTYPE"

    export LC_COLLATE=POSIX # sort in POSIX order.
    export TZ=US/Eastern

    export XDG_DATA_HOME=$HOME/.local/share
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_CACHE_HOME=$HOME/.cache

    path=(
        ~/bin
        /home/dylan/.gem/ruby/*/bin(N)
        /usr/local/bin
        /usr/bin
        /bin
        /usr/local/sbin
        /usr/sbin
        /sbin
        $path
    )
    perl5lib=(~/lib 'lib')
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh/lib,~/:
