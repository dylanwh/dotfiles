{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.bash = {
    enable = true;

    historyControl = [
      "ignoreboth"
    ];
    historySize = 1000;
    historyFileSize = 2000;

    shellOptions = [
      "histappend"
      "checkwinsize"
      "globstar"
    ];

    shellAliases = {
      ls = "${
        if pkgs.stdenv.isDarwin then "${pkgs.coreutils}/bin/ls" else "ls"
      } -Fh --color=auto --group-directories-first";
      ll = "ls -alF";
      la = "ls -A";
      l = "ls -CF";
      grep = "grep --color=auto";
      fgrep = "fgrep --color=auto";
      egrep = "egrep --color=auto";
      sudo = "env -u TERMINFO TERM=xterm-256color sudo";
    };

    initExtra = ''
      if command -v fish &>/dev/null && (( SHLVL < 2 )); then
        fish -i
        exit $?
      fi

      [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

      export LESS_TERMCAP_mb=$'\e[1;34m'
      export LESS_TERMCAP_md=$'\e[1;34m'
      export LESS_TERMCAP_me=$'\e[0m'
      export LESS_TERMCAP_se=$'\e[0m'
      export LESS_TERMCAP_so=$'\e[7;33m'
      export LESS_TERMCAP_ue=$'\e[0m'
      export LESS_TERMCAP_us=$'\e[4;36m'
      export _NROFF_U=1
      export GROFF_NO_SGR=1
    '';
  };
}
