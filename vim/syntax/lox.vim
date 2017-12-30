if exists("b:current_syntax")
  finish
endif

syn keyword loxBoolean true false

syn match loxComment "\v\/\/.*$"

syn keyword loxConditional if else and or

syn keyword loxConstant nil

syn match loxFunctionName "\v\i+" display contained

syn keyword loxKeyword for return super this var while
syn keyword loxKeyword fun nextgroup=loxFunctionName skipwhite skipempty skipnl

syn match loxNumber "\v<\d+>"
syn match loxNumber "\v<\d+\.\d+>"

syn match loxOperator "\v\+"
syn match loxOperator "\v\-"
syn match loxOperator "\v\*"
syn match loxOperator "\v[^/]/[^/]"
syn match loxOperator "\v\="
syn match loxOperator "\v\!\="
syn match loxOperator "\v\<"
syn match loxOperator "\v\>"

syn keyword loxPrint print

syn region loxString start='"' end='"'

syn region  loxClassDefinition   start="\v<class>" end="\v\_[^{]*" contains=loxClassKeyword,loxClassName,loxClassExtendsToken nextgroup=loxClassBlock skipwhite skipempty skipnl
syn keyword loxClassKeyword      contained class
syn match   loxClassName         contained "\v\i+"
syn match   loxClassExtendsToken contained '<'
syn region  loxClassBlock        contained start='{' end='}' contains=loxMethodName
syn match   loxMethodName        contained "\v\i+" skipwhite skipempty nextgroup=loxMethodArguments
syn region  loxMethodArguments   contained start='(' end=')' skipwhite skipempty nextgroup=loxMethodBlock
syn region  loxMethodBlock       contained start='{' end='}' contains=ALLBUT,loxClassKeyword,loxClassName,loxClassExtendsToken,loxClassBlock,loxMethodName,loxMethodArguments,loxFunctionName

hi link loxBoolean      Boolean
hi link loxComment      Comment
hi link loxConditional  Conditional
hi link loxConstant     Constant
hi link loxFunctionName Function
hi link loxKeyword      Keyword
hi link loxNumber       Number
hi link loxOperator     Operator
hi link loxPrint        Function
hi link loxString       String

hi link loxClassKeyword Keyword
hi link loxClassName    Type
hi link loxMethodName   Function

let b:current_syntax = "lox"
