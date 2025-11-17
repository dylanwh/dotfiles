#!zsh

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_ALL_DUPS  # Remove older duplicate entries from history
setopt HIST_REDUCE_BLANKS    # Remove superfluous blanks from history
setopt SHARE_HISTORY         # Share history between sessions
setopt INC_APPEND_HISTORY    # Write to history immediately, not on shell exit

autoload -Uz compinit && compinit

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Use menu selection (like Fish)
zstyle ':completion:*' menu select

# Colorful completion (directories in blue, etc.)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

setopt AUTO_CD              # cd by typing directory name if it's not a command
setopt AUTO_PUSHD           # Make cd push old directory onto directory stack
setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of same directory
setopt PUSHD_SILENT         # Don't print directory stack after pushd/popd

bindkey -e  # Emacs-style keybindings (like Fish default)

# Ctrl+Left/Right to move by word
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# Home/End keys
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line

# Delete key
bindkey "^[[3~" delete-char

# Ctrl+R for history search (already works but explicit)
bindkey "^R" history-incremental-search-backward

setopt CORRECT              # Command correction
setopt COMPLETE_IN_WORD     # Complete from both ends of word
setopt ALWAYS_TO_END        # Move cursor to end after completion

# Don't beep
unsetopt BEEP

export CLICOLOR=1

if which starship >&/dev/null; then
    source =(starship init zsh)
fi
