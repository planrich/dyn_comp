unit prelude

func id: a -> a
= value ; value

func head: [a] -> a
= (l:ls) ; l
= [] ; 1

func tail: [a] -> [a]
= (l:ls) ; ls
= [] ; 1

func map: (a -> b) -> [a] -> [b]
= f [] ; 1
= f (l:ls) ; f l : map f ls

