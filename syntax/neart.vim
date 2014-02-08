syntax match String /"[^"]*"/
syntax match String /"[^"]*\n/
syntax match String /'[^']*'/

" Numbers
syntax match Number /\d\+\.\?\d*/
syntax match Number /0x[\da-fA-F]\+/
syntax match Number /0b[01]\+/

syntax match Special /^=/
syntax match Special /;/
syntax match Special /->/

syntax match Function /\s*[a-zA-Z0-9_]*:/

syntax keyword Keyword func
syntax keyword Keyword data
syntax keyword Keyword unit
syntax keyword Keyword let
syntax keyword Keyword in

syntax keyword Conditional if then else
syntax keyword Operator "=", ";", ":", "*", "+", "-", "/"

syntax match Comment /\-\-.*/
