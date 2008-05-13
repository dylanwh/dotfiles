" To make this file do stuff, add something like the following (without the
" leading ") to your ~/.vimrc:
" au BufNewFile,BufRead *.yaml,*.yml so ~/src/PyYaml/YAML.vim

" Vim syntax/macro file
" Language:	YAML
" Author:	Igor Vergeichik <iverg@mail.ru>
" Sponsor: Tom Sawyer <transami@transami.net>
" Stayven: Ryan King <jking@panoptic.com>
" Copyright (c) 2002 Tom Saywer

" Add an item to a gangly list:
map , o<bs><bs><bs><bs>-<esc>o
" Convert to Canonical form:
map \c :%!python -c 'from yaml.redump import redump; import sys; print redump(sys.stdin.read()).rstrip()'

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syntax clear

syn match yamlStream	"\s*---$"
syn region yamlComment	start="\s*\#" end="$"
syn match yamlDelimiter	"[:,]"
syn match yamlBlock "[\[\]\{\}]"

syn region yamlString	start="'" end="'" skip="\\'"
syn region yamlString	start='"' end='"' skip='\\"' contains=yamlEscape

syn match  yamlKey		"\w\+\ze\s*:"
syn match  yamlType		"![^\s]\+\s\@="

hi link yamlKey		Identifier
hi link yamlComment	Comment
hi link yamlStream	Statement
hi link yamlBlock	Operator
hi link yamlDelimiter	Delimiter
hi link yamlString	String

