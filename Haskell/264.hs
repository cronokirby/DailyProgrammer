import Data.List
import System.IO
import System.Environment

type Code = String
data Line = Line { indentation :: Int   --Can be manipulated for priority
                 , sortPriority :: Int
                 , code :: String       --To extract the code after sorting
                 } deriving (Show, Eq)

compareLines :: Line -> Line -> Ordering
compareLines line1 line2
    | indentation1 > indentation2  = GT
    | indentation1 < indentation2  = LT
    | indentation1 == indentation2 = compare (sortPriority line1)
                                             (sortPriority line2)
    where
      indentation1 = indentation line1
      indentation2 = indentation line2


makeLine :: String -> Line
makeLine line =
    let (spaces, code) = span (== ' ') line
        priority = prioritize code
        indentation = indentationLevel code spaces
    in Line {indentation = indentation, sortPriority = priority, code = line}

indentationLevel :: Code -> String -> Int
indentationLevel "}" spaces
    | null spaces = 100
    | otherwise   = length spaces +  2
indentationLevel code spaces
    | "return" `isPrefixOf` code = length spaces + 2
    | "std::"  `isPrefixOf` code = length spaces + 2
    | otherwise                  = length spaces

prioritize :: Code -> Int
prioritize code
    | "#include" `isPrefixOf` code = 0
    | "{"        `isPrefixOf` code = 2
    | "}"        `isPrefixOf` code = 3
    | "return"   `isPrefixOf` code = 5
    | "std::"    `isPrefixOf` code = 4
    | otherwise                    = 1


main :: IO ()
main = do
    (fileName : args) <- getArgs
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        let codelines = map makeLine $ lines contents
        putStrLn $ unlines $ map code $ sortBy compareLines codelines)
