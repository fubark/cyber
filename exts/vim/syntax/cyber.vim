" Match language specific keywords
syntax keyword cyberKeyword
    \ if then else match while for each break continue pass
    \ or and not is
    \ var static capture as
    \ func return
    \ coinit coyield coresume
    \ object atype tagtype true false none
    \ try catch recover
    \ import export

syntax match cyberComment "--.*$"

" Set highlights
highlight default link cyberKeyword Keyword
highlight default link cyberComment Comment

