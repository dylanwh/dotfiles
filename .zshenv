# Dylan William Hardison's .zshenv file. #'
# .zshenv should not contain commands that produce output or require a tty.
# See also: [~/.zshenv] ~/.zprofile ~/.zshrc ~/.zlogin ~/.zlogout

emulate zsh

declare -gxT PERL5LIB perl5lib # declare array
declare -U path perl5lib       # remove duplicates
setopt noglobalrcs

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

    for plugin_dir in ~/.zsh/bundle/*(N/); do
        plugin_name="$(basename $plugin_dir)"
        plugin_files=($plugin_dir/($plugin_name.plugin.zsh|$plugin.zsh|init.zsh|*.zsh)(N.))
        if (( $#plugin_files > 0 )); then
            for plugin_file in $plugin_files; do
                source $plugin_file
            done
            unset plugin_file
        else
            fpath+=( $plugin_dir )
        fi
    done

    unset plugin_dir plugin_files plugin_name

    for file in $HOME/app/*/zshenv(N); do
        source $file
    done
    unset file

fi

# vim: set sw=4 ts=4 foldmethod=marker path=.,~/.zsh:
