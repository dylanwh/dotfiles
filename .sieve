# vim: set ft=sieve:
require "fileinto";
#if address :is "to" "dylan@njord.hardison.net" {
#  	fileinto "pants";
#} 

#if address :domain :is "to" "r-stream.com" {
#	fileinto "work";
#}
#elsif header :contains "List-Id" "haskell-cafe.haskell.org" {
#    fileinto "lists.haskell-cafe";
#}
#elsif header :contains "List-Id" "dwm.suckless.org" {
#	fileinto "lists.dwm";
#}
#elsif header :contains "List-Id" "caml-list.yquem.inria.fr" {
#	fileinto "lists.caml";
#}
#elsif header :contains "List-Id" "module-authors.perl.org>" {
#	fileinto "lists.module-authors";
#}
#elsif header :is "Sender" "slug@nks.net" {
#	fileinto "lists.slug";
#}
