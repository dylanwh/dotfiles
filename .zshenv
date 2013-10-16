# Dylan William Hardison's .zshenv file. #'
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

emulate zsh

declare -gxT PERL5LIB perl5lib # declare array
declare -U path perl5lib       # remove duplicates

if [[ -o rcs ]]; then
    export REALNAME="Dylan William Hardison"
    export EMAIL="dylan@hardison.net"
    export EDITOR="vim"
    export VISUAL="$EDITOR"
    export BROWSER="chrome"
    export MANPAGER='less -s'

    export HOST="${HOST/.*/}"
    export OSTYPE="$OSTYPE"

    export LC_COLLATE=POSIX # sort in POSIX order.
    export TZ=US/Eastern

    export XDG_DATA_HOME=$HOME/.data
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_CACHE_HOME=$HOME/.cache

    path=(
        ~/bin
        ~/app/*/bin(N)
        /home/dylan/.gem/ruby/*/bin(N)
        /usr/local/bin
        /usr/bin
        /bin
        /usr/local/sbin
        /usr/sbin
        /sbin
        $path
    )
    fpath=(~/.zsh/lib $fpath)
    perl5lib=(~/lib 'lib')

    for file in $HOME/app/*/zshenv(N); do
        source $file
    done

    export PERLBREW_HOME=$HOME/app/perlbrew
    export PERLBREW_ROOT=$PERLBREW_HOME
    unset PERLBREW_PATH

    if which perlbrew &> /dev/null; then
        [[ ! -d $PERLBREW_HOME ]] && perlbrew init
        [[ -f $PERLBREW_HOME/etc/bashrc ]] && source $PERLBREW_HOME/etc/bashrc
    fi
fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
