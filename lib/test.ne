unit prelude

func main: int
=; sum 10


func sum: int -> int
= a ; if a == 0 then 0
      else (sum (a-1)) + a

