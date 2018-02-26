-- This is a single line comment

{-
This
is
a
multi
line
comment
-}

-- Module imports:
import Data.List
import System.IO

-- Data types:
-- Int (Whole numbers, -2^63 to 2^63)
-- -- maxBound :: Int and minBound :: Int are the max and min bounds
-- Integer (unbounded whole numbers, as big as memory can hold)
-- Float (single precision floating point)
-- Double (Precision up to 11 points after the decimal)
-- Bool - True or False
-- Char - ' (single quotes)
-- Tuple - List of many different data types

-- Constants:
-- Ex.
always5 :: Int
always5 = 5

-- Math Functions:
sumOfNums = sum[1..1000]
addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4 -- mod is a prefix operator
modEx2 = 5 `mod` 4 -- now, mod is an infix operator

negNumEx = 5 + (-4)


-- Square root
-- Note: :t gives the type
-- sqrt has type Floating a => a -> a, so we must give it a float as input!
num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

-- Logical operators
trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)


-- Lists
primeNumbers = [3,5,7,11]
morePrimes = primeNumbers ++ [13, 17, 19, 23, 29] -- concatenation
favNums = 2 : 7 : 21 : 66 : [] -- colon operator
multList = [[3,5,7],[11,13,17]]

morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2
revPrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removedPrimes = drop 3 morePrimes2
is7InList = 7 `elem` morePrimes2
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

newList = [2,3,5]
productNewList = product newList

zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']

infinPow10 = [10, 20..] -- infinite list (lazy)

many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]
divisBy9N13 = [x | x <-[1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedlist = sort [9,1,4,5,8,2]

sumOfList = zipWith (+) [1,2,3,5,6] [1,2,34,6,6]

listBiggerThen5 = filter (>5) morePrimes

evensUpTo20 = takeWhile (<=20) [2,4..]

multOfList = foldl (*) 1 [2,3,4,5]
multOfListR = foldr (*) 1 [2,3,4,5]

-- More on list comprehensions
pow3List = [3^n | n <-[1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]


-- Tuples (list of multiple different data types)
randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52) -- Tuple pair
bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary" , "Tom"]
addresses = ["123 Main", "234 North", "567 South"]
namesNAddresses = zip names addresses

-- Functions
-- In ghc:
-- -- let num7 = 7
-- -- let getTriple x = x * 3
-- -- getTriple num7

-- main = do
--   putStrLn "What's your name"
--   name <- getLine
--   putStrLn ("Hello " ++ name)

-- to compile and run:
-- ghc --make EXECUTABLENAME

addMe :: Int -> Int -> Int
addMe x y = x + y

-- We can have Haskell infer the type
-- This function will work with floats!
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1,y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge  :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "you can vote"
whatAge 21 = "you're an adult"
whatAge x = "nothing important"
-- likewise:
-- whatAge _ = "nothing important" also works


-- Factorials
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

prodFact n = product [1..n]

-- Guards
isOdd :: Int -> Bool
-- This is the guard part (sort of like an if statement!)
isOdd n
  | n `mod` 2 == 0 = False
  | otherwise = True

whatGrade :: Int -> String
whatGrade age
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age > 6) && (age <= 10) = "Elementary School"
  | (age > 10) && (age <= 14) = "Middle School"
  | (age > 14) && (age <= 18) = "High School"
  | otherwise = "Go to colllege"


-- where clause (helps us repeat calculations)
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible Batting Average"
  | avg <= 0.250 = "Average Player"
  | avg <= 0.280 = "You're doing pretty good"
  | otherwise = "You're a superstar"
  where avg = hits / atBats

getListItems:: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "your list starts with " ++ show x
getListItems (x:y:[]) = "your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

-- higher order function
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [ 1,2,3,4,5]

-- We can make our own map!
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs


areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Time4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3
fourPlus3 = adds3 4

threePluslist = map adds3 [1,2,3,4]

-- Lambdas
dbl1tTo10 = map (\x -> x * 2) [1..10]

-- Conditionals
-- < > <= >= == /=

-- If statements (not used much)
doubleEvenNumber y =
    if (y `mod` 2 /= 0)
      then y
      else y * 2

-- case statements
getClass :: Int -> String
getClass n = case n of
  5 -> "go to Kindergarten"
  6 -> "blah"
  _ -> "foo"

-- Modules
-- Ex. module SampFunctions (getClass, doubleEvenNumbers) where

-- Enumeration Types:
data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                   deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print (barryBonds Outfield)

-- Custom data types
data Customer = Customer String String Double
  deriving Show

tomSmith ::  Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b


data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Paper Rock = "paper beats rock"
-- add other possibilites
shoot _ _ = "Error" -- Catch all

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) *  (abs $ y2 - y)
-- $ means everything after it takes precedence over anything that comes before it
-- Likewise we can put:
-- area (Rectangle x y x2 y2) = (abs (x2 - x)) *  (abs (y2 - y))

-- Dot operator (chains functions)
sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100

-- Type Classes
-- Num Eq Or Show
-- Sets of types that have certain operations defined for them

-- Ex. (+) - Works with parameters that have type Num

data Employee = Employee { name :: String,
                           position :: String,
                           idNum  :: Int
                         } deriving (Eq, Show)

samSmithEmployee = Employee {name = "Sam Smith", position = "Manager", idNum = 1001}
billyBobEmployee = Employee {name = "Billy Bob", position = "Sales", idNum = 1002}

isSamBilly = samSmithEmployee == billyBobEmployee
samSmithData = show samSmithEmployee

-- Notice we are now using capital letters for the first letter!
-- (This is why we can't use capitals for ftn names)
data ShirtSize = S | M | L

instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvailable = S `elem` [S, M, L]
theSize = show S


-- Custom Type Classes
class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

newSize = areEqual M M

-- I/O
-- Write
writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("randomline of text")
  hClose theFile

-- Read
readToFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile2
  putStr contents
  hclose theFile
