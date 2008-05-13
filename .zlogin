# Dylan William Hardison's .zlogin file.
# This is the last script executed before user interaction.
# See also: ~/.zshenv ~/.zprofile ~/.zshrc [~/.zlogin] ~/.zlogout

# run other components -- zsh is a bourne shell

if [[ -o interactive ]]; then
	sshbegin
	uptime
fi
