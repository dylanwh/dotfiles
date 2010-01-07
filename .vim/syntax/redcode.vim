" Vim syntax file
" Language: REDCODE
" Maintainer:   Philip Thorne <pbt@pipeline.com>
" Last Change:  2006 Nov 04
"
" Remark: Language per REDCODE Reference Standard: ICWS'94 draft (extended)
" Remark:   http://www.koth.org/info/pmars-redcode-94.txt
"
" Options to control REDCODE high-lighting
"   For 88 syntax only:
"     let redcode_88_only = 1
"
"   For 94 syntax only (superset of 88 syntax):
"     let redcode_94_only = 1
"
"   To highlight numbers
"     let redcode_highlight_numbers=1


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

"Opcodes
syn keyword red88Operator DAT MOV ADD SUB JMP JMZ JMN DJN SLT CMP SPL
syn keyword red94Operator MUL DIV MOD SEQ SNE NOP
syn keyword redPOperator LDP STP

"Pseudo opcodes
syn keyword red88PseudoOperator EQU END FOR ROF
syn keyword red94PseudoOperator ORG
syn keyword redPPseudoOperator PIN

"Predefined variables
syn case match
syn keyword red88Constant CORESIZE MAXPROCESSES MAXCYCLES MAXLENGTH
syn keyword red88Constant MINDISTANCE ROUNDS CURLINE VERSION WARRIORS
syn keyword redPConstant PSPACESIZE
syn case ignore

syn match redDirective "\<redcode\([^-]\|$\)" contained
syn keyword redDirective assert author break name contained
syn match redDirective "debug[ \t]\{1,}\(off\|static\)" contained
syn match redDirective "trace[ \t]\{1,}off" contained

syn match redLineContinue "\\$" contained

syn match redComment    ";.*$" contains=redDirective,redKOTHDirective,redLineContinue,red88Constant,redPConstant

syn match redLabel      "^\b*[a-zA-Z_][a-zA-Z0-9_]*"
syn match redIdentifier "\<[a-zA-Z_][a-zA-Z0-9_]*\>"

syn match red88AddressingMode "[#$@<]"
syn match red94AddressingMode "[>*{}]"

syn match redModifier   "\.\([ABFXI]\|AB\|BA\)\>"

"Number high-lighting seems cluttered
if exists( "redcode_highlight_numbers" )
  syn match redNumber   "\<\d*\>"
endif

"Directives for http://koth.org server
syn keyword redKOTHDirective help kill status strategy test contained
syn keyword redKOTHDirective password newpasswd newredcode url version contained
syn match redKOTHDirective "\<redcode-[a-zA-Z0-9]\{1,}\>" contained

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_red_syntax_inits")
  if version < 508
    let did_red_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  if exists( "redcode_88_only" )
    HiLink red88Operator        Statement
    HiLink red88AddressingMode  Special
    HiLink red88PseudoOperator  PreProc
    HiLink red88Constant        Constant

    HiLink red94Operator        Error
    HiLink redPOperator         Error
    HiLink redModifier          Error
    HiLink red94AddressingMode  Error
    HiLink red94PseudoOperator  Error
    HiLink redPPseudoOperator   Error
    HiLink redPConstant         Error

  elseif exists ( "redcode_94_only" )
    HiLink red88Operator        Statement
    HiLink red94Operator        Statement
    HiLink redModifier          Operator
    HiLink red88AddressingMode  Special
    HiLink red94AddressingMode  Special
    HiLink red88PseudoOperator  PreProc
    HiLink red94PseudoOperator  PreProc
    HiLink red88Constant        Constant

    HiLink redPOperator         Error
    HiLink redPPseudoOperator   Error
    HiLink redPConstant         Error

  else
    HiLink red88Operator        Statement
    HiLink red94Operator        Statement
    HiLink redPOperator         Statement
    HiLink redModifier          Operator
    HiLink red88AddressingMode  Special
    HiLink red94AddressingMode  Special
    HiLink red88PseudoOperator  PreProc
    HiLink red94PseudoOperator  PreProc
    HiLink redPPseudoOperator   PreProc
    HiLink red88Constant        Constant
    HiLink redPConstant         Constant
  endif

  HiLink redComment             Comment
  HiLink redDirective           SpecialComment
  HiLink redKOTHDirective       SpecialComment

  HiLink redIdentifier          Identifier
  HiLink redLabel               Identifier

  if exists( "redcode_highlight_numbers" )
    HiLink redNumber            Number
  endif

  "Handy to avoid Marcia Trionfale bug
  HiLink redLineContinue        WarningMsg
    
  delcommand HiLink
endif

let b:current_syntax = "red"

" vim: ts=4
