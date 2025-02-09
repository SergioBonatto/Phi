" Vim syntax file
" Language: Phi

if exists("b:current_syntax")
  finish
endif

" Variable names (start with lowercase)
syntax match phiVarName '\v<[a-z][a-zA-Z0-9_]*>'

" Lambda expressions
syntax match phiLambda 'λ'
syntax match phiDot '\.'

" Let definitions
syntax keyword phiKeyword let

" Special characters
syntax match phiSpecialChar '[()=]'

" Comments
syntax match phiCommentLine '--.*$'
syntax region phiCommentBlock start='{-' end='-}'

" Strings
syntax region phiString start='"' end='"' contains=phiStringEscape
syntax match phiStringEscape '\\.' contained

" Lambda shortcut (preserved as requested)
nnoremap \l iλ<ESC>
inoremap \l λ

" Highlight Links
highlight default link phiVarName Identifier
highlight default link phiLambda Special
highlight default link phiDot Operator
highlight default link phiKeyword Keyword
highlight default link phiSpecialChar Delimiter
highlight default link phiCommentLine Comment
highlight default link phiCommentBlock Comment
highlight default link phiString String
highlight default link phiStringEscape Special

let b:current_syntax = 'phi'
