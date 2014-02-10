unit prelude

func id2: int -> int
= value ; value

func id3: str -> str -> bin
= value ; value

func id: a -> a
= value ; value

func head: [a] -> a
= (l:ls) ; l
= [] ; error

func tail: [a] -> [a]
= (l:ls) ; ls
= [] ; []

func add: a
=; 1 + 133 * 2 / 4 - -2
=; (1 + ((133 * 2) / 4)) - -2

func map: (a -> b) -> (c -> d) -> [a] -> [b]
= f [] ; []
= f (l:ls) ; ( f l : (map f ls) )
=; get i + 13

