syntax match String /"[^"]*"/
syntax match String /"[^"]*\n/
syntax match String /'[^']*'/

" Numbers
syntax match Number /\d\+\.\?\d*/
syntax match Number /0x[\da-fA-F]\+/
syntax match Number /0b[01]\+/

syntax match Keyword /^=/

syntax keyword Keyword fn 

syntax keyword Conditional if then else let in
syntax keyword Operator "=", ";", ":"

syntax match Comment /\-\-.*/
