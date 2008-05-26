# vim: set ft=sieve:
require "fileinto";
#if address :is "to" "dylan@njord.hardison.net" {
#  	fileinto "pants";
#} 

if header :contains "List-Id" "haskell-cafe.haskell.org" {
    fileinto "lists.haskell-cafe";
}

if header :contains "List-Id" "dwm.suckless.org" {
	fileinto "lists.dwm";
}

if header :contains "Sender" "slug@nks.net" {
	fileinto "lists.slug";
}

