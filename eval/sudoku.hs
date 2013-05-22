import Data.List
import System.Environment

type Row = [Integer]
type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)
    
isSymmetricMatrix :: [[Integer]] -> Bool -- check if sudoku is sym.
isSymmetricMatrix m = and $ map (\x -> ((length x) == n)) m
	where n = length m
	     
	      
isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK sudoku variant
	| not $ isSymmetricMatrix sudoku = False
	--consider all variants
	| variant == Basic = minimum
	| variant == Cross = minimum && cross
	| variant == Color = minimum && color
	where h = map (\x -> sudokuBlockValid sudoku [(x,y) | y <- [1..9]] []) [1..9]
	      v = map (\x -> sudokuBlockValid transposed [(x,y) | y <- [1..9]] []) [1..9]
	      blocks = [ sudokuBlockValid sudoku (concat $ map (\x -> map (\y -> (x,y)) a) b) [] | a <- [[1,2,3],[4,5,6],[7,8,9]], b <- [[1,2,3],[4,5,6],[7,8,9]]]
	      minimum = and h && and v && and blocks
	      cross = sudokuBlockValid sudoku [(a,a) | a <- [1..9]] [] && sudokuBlockValid transposed [(a,a) | a <- [1..9]] []
	      color = and [ sudokuBlockValid sudoku (colorPos x y) [] | x <- [1..3], y <- [1..3]]
	      transposed = transpose $ reverse sudoku
     
type X = Integer
type Y = Integer

colorPos :: X -> Y -> [(X,Y)]
colorPos x y = [(row,col) | row <- take 3 (iterate (+3) x), col <- take 3 (iterate (+3) y)]
     
--check if a given block is valid go over the list of (x,y)
sudokuBlockValid :: Sudoku -> [(X,Y)] -> [Integer] -> Bool
sudokuBlockValid sudo [] _ = True
sudokuBlockValid sudo ((a,b):cs) collected
	| not $ elem a [1..9] = error "a not in range of 1-9"
	| not $ elem b [1..9] = error "b not in range of 1-9"
	| elem number  [1..9] = if (not $ elem number collected) then sudokuBlockValid sudo cs (number:collected) else False
	| otherwise = sudokuBlockValid sudo cs collected
	where number = numberAt sudo a b

-- get number at sudoku with x and y
numberAt :: Sudoku -> X -> Y -> Integer
numberAt sudo x y = ((sudo !! (fromIntegral (x - 1))) !! (fromIntegral (y - 1))) 

--inverse of findunused, not used anyway
findUsed :: Sudoku -> Y -> [(X,Y)]
findUsed [] _ = []
findUsed (s:ss) y = filter (\x -> if x == (0,0) then False else True) (map (\x -> if (s !! x) /= 0 then ((toInteger (x+1)),(y+1)) else (0,0)) [0..8]) ++ findUnused ss (y+1)

--find all free places
findUnused :: Sudoku -> Y -> [(X,Y)]
findUnused [] _ = []
findUnused (s:ss) y = filter (\x -> if x == (0,0) then False else True) (map (\x -> if (s !! x) == 0 then ((toInteger (x+1)),(y+1)) else (0,0)) [0..8]) ++ findUnused ss (y+1)
-- ? can else (0,0) be avoided

--replace x,y with with
replace :: Sudoku -> X -> Y -> Integer -> Sudoku
replace sudo x y with = top ++ [rep] ++ bot
	where top = take (fromIntegral (y-1)) sudo
	      bot = drop (fromIntegral y) sudo
	      yline = sudo !! (fromIntegral (y-1))
	      left = take (fromIntegral (x-1)) yline
	      rep = left ++ [with] ++ (drop (fromIntegral x) yline)
	
--solve the sudoku, build the tree go over it and if there is some valid sudoku
--return it builded with replaceAll
solve :: Sudoku -> Variant -> Maybe Sudoku
solve sudo variant
	| foundit == [] = Nothing
	| otherwise = Just (replaceAll sudo foundit)
	where unused = findUnused sudo 0
	      foundit = findALeaf (buildSDKTree sudo (findUnused sudo 0) variant) []
	
--replace all x,y with i in list then return the sudoku
--this is used at the end to generate valid soduku      
replaceAll :: Sudoku -> [((X,Y),Integer)] -> Sudoku
replaceAll s [] = s
replaceAll sudo ( ((x,y),i) : ls) = replaceAll rep ls
	where rep = replace sudo x y i
	
data SDKTree = SDK [(((X,Y),Integer),SDKTree)] | SDKLeaf deriving (Show,Eq)

--go deep don into the tree and search for leaf
findALeaf :: SDKTree -> [((X,Y),Integer)] -> [((X,Y),Integer)]
findALeaf SDKLeaf i = i --WOOOT a long way to find one of this
findALeaf (SDK []) _ = []
findALeaf (SDK ((e,tree):trees)) i
	| found /= [] = found
	| otherwise = findALeaf (SDK trees) i
	where found = findALeaf tree (e:i)
	
--build the tree which has a valid way if it ends with a leaf
buildSDKTree :: Sudoku -> [(X,Y)] -> Variant -> SDKTree
buildSDKTree sudo [] var = SDKLeaf
buildSDKTree sudo (i:is) var = SDK content
	where posVals = buildPosSDK sudo (fst i) (snd i) [1..9] var
	      content = map (\x -> (x,buildSDKTree (replace sudo (fst i) (snd i) (snd x)) is var)) posVals
	
 --get all possible replacements for a x and y and return it in a list
buildPosSDK :: Sudoku -> X -> Y -> [Integer] -> Variant -> [((X,Y),Integer)]
buildPosSDK sudo x y [] _ = []
buildPosSDK sudo x y (i:is) var
	| isValidSDK reped var = (((x,y),i):buildPosSDK sudo x y is var)
	| otherwise = buildPosSDK sudo x y is var
	where reped = replace sudo x y i 

main = do
    (arg:_) <- getArgs
    case arg of
        "0" -> handle (solve basic_start1 Basic) (Just [[9,1,6,3,5,4,8,7,2],[8,7,3,6,2,9,1,5,4],[5,2,4,7,1,8,9,3,6],[7,6,8,9,3,5,2,4,1],[1,4,9,2,8,7,3,6,5],[2,3,5,4,6,1,7,9,8],[6,9,7,8,4,2,5,1,3],[3,8,1,5,7,6,4,2,9],[4,5,2,1,9,3,6,8,7]])
        "1" -> handle (solve basic_start2 Basic) (Just [[6,2,4,3,5,7,1,8,9],[9,7,1,6,2,8,3,5,4],[8,3,5,4,9,1,7,2,6],[5,6,3,8,7,2,9,4,1],[2,8,9,1,3,4,6,7,5],[4,1,7,5,6,9,2,3,8],[3,4,6,2,1,5,8,9,7],[7,5,2,9,8,6,4,1,3],[1,9,8,7,4,3,5,6,2]])
        "2" -> handle (solve basic_start3 Basic) (Just [[9,7,6,5,1,3,2,4,8],[1,5,8,6,4,2,7,9,3],[2,3,4,7,9,8,5,1,6],[7,6,1,8,3,5,9,2,4],[8,2,3,4,7,9,1,6,5],[5,4,9,1,2,6,8,3,7],[6,9,7,3,5,1,4,8,2],[3,1,5,2,8,4,6,7,9],[4,8,2,9,6,7,3,5,1]])
        "3" -> putStrLn $ "evaluated: " ++ (show (solve basic_start4 Basic))
        "4" -> putStrLn $ "evaluated: " ++ (show (solve basic_start5 Basic))
        "5" -> putStrLn $ "evaluated: " ++ (show (solve basic_start6 Basic))
        "6" -> putStrLn $ "evaluated: " ++ (show (solve basic_start7 Basic))

handle s1 s2
    | s1 == s2 = putStrLn $ "evaluated: " ++ (show s1)
    | otherwise = putStrLn "failed!"
 
basic_start1 = [[9,1,6,0,0,4,0,7,2],
                [8,0,0,6,2,0,0,5,0],
                [5,0,0,0,0,8,9,3,0],
                [0,6,0,0,0,0,2,0,0],
                [0,0,0,2,0,7,0,0,0],
                [0,0,5,0,0,0,0,9,0],
                [0,9,7,8,0,0,0,0,3],
                [0,8,0,0,7,6,0,0,9],
                [4,5,0,1,0,0,6,8,7]]

basic_start2 = [[6,0,0,3,0,0,1,0,0],
                [0,7,1,6,2,0,0,0,0],
                [8,0,5,0,0,1,0,0,0],
                [5,0,0,8,7,0,9,0,1],
                [0,0,9,0,0,0,6,0,0],
                [4,0,7,0,6,9,0,0,8],
                [0,0,0,2,0,0,8,0,7],
                [0,0,0,0,8,6,4,1,0],
                [0,0,8,0,0,3,0,0,2]]


basic_start3 = [[9,0,6,0,1,3,0,0,8],
                [0,5,8,0,0,0,0,9,0],
                [0,3,0,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,1,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]

basic_start4 = [ [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]

      , [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]

      , [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]
      , [0,0,0, 0,0,0, 0,0,0]
      ]
basic_start5 =  [ [0,0,4, 0,0,0, 7,0,0]
      , [6,0,7, 0,1,8, 0,4,9]
      , [0,9,0, 4,0,0, 0,3,0]

      , [0,0,9, 0,0,7, 0,0,0]
      , [4,0,0, 0,0,3, 0,0,5]
      , [0,0,0, 8,0,0, 0,0,0]

      , [0,7,0, 0,0,2, 4,9,0]
      , [2,0,0, 0,9,0, 0,0,6]
      , [9,0,5, 0,0,0, 3,0,0]
      ]
basic_start6 =  [ [3,0,5, 0,8,7, 0,9,0]
      , [0,9,7, 0,0,1, 6,0,3]
      , [6,0,8, 9,3,2, 5,7,1]

      , [8,5,0, 0,1,4, 0,2,0]
      , [7,2,1, 0,9,6, 8,0,5]
      , [4,0,0, 8,0,5, 9,1,7]

      , [0,3,0, 2,4,8, 7,0,0]
      , [5,7,2, 1,0,9, 4,3,8]
      , [9,8,0, 0,7,3, 0,6,2]
      ]
basic_start7 =  [ [0,0,7, 9,0,0, 0,0,0]
      , [0,0,4, 3,0,0, 0,1,0]
      , [0,0,0, 0,1,0, 0,8,9]

      , [6,0,0, 0,0,0, 0,5,1]
      , [1,5,3, 0,0,0, 6,4,7]
      , [4,7,0, 0,0,0, 0,0,3]

      , [3,2,0, 0,8,0, 0,0,0]
      , [0,6,0, 0,0,2, 4,0,0]
      , [0,0,0, 0,0,7, 3,0,0]
      ]
