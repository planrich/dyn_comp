unit prelude

func id: a -> a
= value ; value

func head: [a] -> a
= (l:ls) ; l
= [] ; error

func tail: [a] -> [a]
= (l:ls) ; ls
= [] ; []
