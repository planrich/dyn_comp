unit prelude

func id: a -> a
= value ; value

func head: [a] -> a
= (l:ls) ; l
= [] ; error

func tail: [a] -> [a]
= (l:ls) ; ls
= [] ; []

func add: a -> a -> a
=; 1 + 133 * 2 / 4 - -2
=; (1 + ((133 * 2) / 4)) - -2

func map: (a -> b) -> [a] -> [b]
= f [] ; []
= f (l:ls) ; ( f l : map f ls )

