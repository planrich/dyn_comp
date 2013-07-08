syntax match String /"[^"]*"/
syntax match String /"[^"]*\n/
syntax match String /'[^']*'/

" Numbers
syntax match Number /\d\+\.\?\d*/
syntax match Number /0x[\da-fA-F]\+/
syntax match Number /0b[01]\+/

syntax match Keyword /^=/
syntax match Keyword /^+/

syntax keyword Keyword func
syntax keyword Keyword data
syntax keyword Keyword struct
syntax keyword Keyword mem
syntax keyword Keyword unit
syntax keyword Keyword version
syntax keyword Keyword legacy
syntax keyword Keyword of
syntax keyword Keyword let
syntax keyword Keyword in
syntax keyword Keyword as

syntax keyword Conditional if then else
syntax keyword Operator "=", ";", ":"

syntax match Comment /\-\-.*/
