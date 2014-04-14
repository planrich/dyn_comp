unit prelude

func main: int
=; add (add 1 1) (seven)

func add: int -> int -> int
= a b ; a + b

func seven: int
=; 7

#func map: (a -> b) -> [a] -> [b]
#= f [] ; []
#= f (a:as) ; f a : map f as
