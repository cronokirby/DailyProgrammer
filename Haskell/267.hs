import Data.Char (digitToInt)
import Data.List

digits :: Int -> [Int]
digits = map digitToInt . show

-- To convert a rank to a string that represents that place
rank 1  = "st"
rank 2  = "nd"
rank 3  = "rd"
rank 11 = "th"
rank 12 = "th"
rank 13 = "th"
rank n
    | n < 10    = "th"               -- special cases caught above
    | n < 100   = rank $ n `mod` 10  -- To extract the last digit
    | otherwise = rank $ n `mod` 100 -- To extract the last 2 digits

rankString :: Int -> String
rankString n = show n ++ rank n

makeRanks :: Int -> Int -> String
makeRanks position limit = intercalate ", ". map rankString
                         $ delete position [0..limit]

main :: IO ()
main = do
    putStrLn "Enter a rank:"
    rank <- getLine
    putStrLn "Enter a limit:"
    limit <- getLine
    let ranks = makeRanks (read rank) (read limit)
    putStrLn ranks
