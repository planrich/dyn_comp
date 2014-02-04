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
=; 1 + 133 * 2 / 1 - -2
