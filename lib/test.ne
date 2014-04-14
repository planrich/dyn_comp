unit prelude

func main: int
=; if 2 == 2 
     then if 1 == 2 
            then (add (fifty) (one111)) 
            else add 100 200 
     else 10 + 20

func add: int -> int -> int
= a b ; a + b

func fifty: int
=; 50

func one111: int
=; 1

func three: int
=; 3

#func map: (a -> b) -> [a] -> [b]
#= f [] ; []
#= f (a:as) ; f a : map f as
