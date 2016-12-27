import Data.Char (digitToInt)
import Data.List

digits :: Int -> [Int]
digits = map digitToInt . show

largestDigit :: Int -> Int
largestDigit = maximum . digits

descendingDigits :: Int -> Int
descendingDigits = read . concatMap show . sortBy (flip compare) . digits

ascendingDigits :: Int -> Int
ascendingDigits = read . concatMap show . sort . digits

kaprekar :: Int -> Int
kaprekar 6174 = 0
kaprekar x = 1 + kaprekar nextNum
    where nextNum = descendingDigits x - ascendingDigits x 
