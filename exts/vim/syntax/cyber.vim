" Match language specific keywords
syntax keyword cyberKeyword
    \ if else switch while for each break continue pass
    \ or and not is
    \ var as
    \ let
    \ func return
    \ coinit coyield coresume
    \ type object enum true false none
    \ try catch recover
    \ use

syntax match cyberComment "--.*$"

" Set highlights
highlight default link cyberKeyword Keyword
highlight default link cyberComment Comment

