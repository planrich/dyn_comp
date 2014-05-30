unit prelude

func main: int
=; add 1 1

func add: int -> int -> int
= a b ; add2 (a + b + 1) (a + b + 1)

func add2: int -> int -> int
= a b ; a + b + 2

