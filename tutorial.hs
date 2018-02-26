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
