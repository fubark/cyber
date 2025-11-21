" Match language specific keywords
syntax keyword cyberKeyword
    \ if else switch case while for break continue pass
    \ or and not
    \ var move const global
    \ fn return self
    \ yield
    \ type object struct cstruct cunion enum trait as
    \ true false none undef 
    \ catch error
    \ use

syntax match cyberComment "--.*$"

" Set highlights
highlight default link cyberKeyword Keyword
highlight default link cyberComment Comment

