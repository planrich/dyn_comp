unit prelude version 0000
+ map
+ filter
+ qsort
+ take
+ drop
+ concat
+ foldl
+ foldr

--legacy stdc
--legacy stl

-- data Person:
-- = Person
--     name of string as pname
--     age of int as page
-- 
-- generate person_name, person_age

-- data Maybe:
-- | Just a
-- | Nothing

-- |0         7|8   11|    |
--mem Bitfield: 
--    < 8 bit as header
--    , 4 bit as version
--    , 32 bit as payload
--    >
--  



func take:
= list a ; ptake list a []
-- private method
func ptake:
= n [] list ; if greater_equal n 0
              then list
              else fatal "tried to take to much from a list"
= 0 (l:ls) list ; list
= n (l:ls) list ; if greater n 0
                  then ptake (sub n 1) ls (append list l)
                  else list

func drop:
= _ [] ; []
= 0 list ; list
= n (l:ls) ; if greater n 0
             then drop (sub n 1) ls
             else cons l ls

func concat:
= list [] ; list
= list (r:rs) ; concat (append list r) rs

func foldl:
= f z [] ; z
= f z (l:ls) ; foldl f (f z l) ls

func foldr:
= f z [] ; z
= f z (l:ls) ; f l (foldr f z ls)

func map:
= f []     ; []
= f (l:ls) ; cons (f l) (map f ls)

func filter:
= f [] ; []
= f (l:ls); if f l 
            then append (filter f ls) l
            else filter f ls

func qsort:
= [] ; []
= (p:qs) ; let lesser: qsort (filter (\x -> less x p) qs) in
           let greater: qsort (filter (\x -> greater_equal x p) qs) in
             concat (append lesser p) greater

