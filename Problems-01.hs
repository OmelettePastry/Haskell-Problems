-- ********************
-- PART A
-- ********************

-- partA Helper function
-- ** Determines if a value is even
isEven :: Int -> Bool
isEven a
  | a >= 0 = (mod a 2 == 0)
  | otherwise = False

-- partA - Return a list of positive integers
partA :: [Int]
partA =	filter isEven (iterate (+1) 1)

-- ********************
-- PART	B
-- ********************

-- partB Helper function
-- ** Determines if a value is positive
positive :: Int -> Bool
positive x = (x >= 0)

-- partB - Takes [Int] and returns the doubles of positive
-- elements of the list
partB :: [Int] -> [Int]
partB lst = map (*2) (filter positive lst)

-- ********************
-- PART	C
-- ********************

-- partC Helper function
-- ** Creates a square approximation for a value
squareApprox :: Float -> Float -> Float
squareApprox n x = (x + (n/x))/2.0

-- partC - Take a number and initial guess and return the infinite
-- list of approximations given by using the formula above
partC :: Float -> Float -> [Float]
partC n x = iterate (squareApprox n) x

-- ********************
-- PART	D
-- ********************

-- partD Helper function
-- ** Return the second value of an ordered pair
get2nd :: [(Float,Float)] -> Float
get2nd [] = 0
get2nd ((x,y):xs) = y

-- partD Helper Function
-- ** Determine if an ordered pair is more than
-- ** z values of each other
moreThan :: Float -> (Float, Float) -> Bool
moreThan z (x,y)
  | abs(x-y) > z = True
  | otherwise = False

-- partD - Take a list and number and searches the lst to find
-- two consecutive list elements othat differ by no more than z
partD :: [Float] -> Float -> Float
partD [] z = 0
partD (x:xs) z =  get2nd (dropWhile (moreThan z) (zip (x:xs) xs))

-- ********************
-- PART	E
-- ********************

partE :: Float -> Float -> Float
partE x t =  partD (partC x 1) t

-- ********************
-- PART	F
-- ********************

-- partF Helper function
-- ** Applies the function to the value in the ordered pair
applyFunc :: (a -> b, a) -> b
applyFunc (x,y) = x y

-- partF - Take a list of functions and a list of values
-- and applies the 1st function to the 1st value, and
-- the 2nd function to the 2nd value, and so on
partF :: [(a -> b)] -> [a] -> [b]
partF funcList list = map applyFunc (zip funcList list) 

-- ********************
-- PART	G
-- ********************

-- partG Helper function
-- ** Applies a function to a value
applyToVal :: a -> (a -> b) -> b
applyToVal x func = func x

-- partG - Takes a list of functions and a value and
-- returns a list of results by applying the functions
-- to that value
partG x fList = map (applyToVal x) fList

-- ********************
-- PART	H
-- ********************

-- partH - Removes all the occurences of the first argument
-- from the argument list
-- partH :: Eq a =>  a -> [a] -> [a]

partH x list = filter (/= x) list

partI :: Eq a => [a] -> [a] -> [a]
partI [] [] = []
partI list1 list2 = foldr (partH) list2 list1

partJ list = helperJ list list

helperJ [][] = []
helperJ list [] = []
helperJ [] list = [list]
helperJ (y:ys) (x:xs) = (foldr (partH) (x:xs) (y:ys)):(helperJ (ys) (x:xs))
