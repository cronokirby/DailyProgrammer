import Data.List

main = do
    line1 <- getLine
    line2 <- getLine
    mapM_ putStrLn $ slowChange line1 line2

slowChange xs ys = zipWith (++) (inits ys) (tails xs)
