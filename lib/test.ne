unit prelude

func test: int -> int -> int
= 1 3 ; 4
= 2 4 ; 6
= a b ; a + b

func id: a -> a
= value ; value

func head: [a] -> a
= (l:ls) ; l
= [] ; error

func tail: [a] -> [a]
= (l:ls) ; ls
= [] ; []

func map: (a -> b) -> [a] -> [b]
= f [] ; []
= f (l:ls) ; ( f l : (map f ls) )
