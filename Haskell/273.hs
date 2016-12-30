--https://www.reddit.com/r/dailyprogrammer/comments/
--4q35ip/20160627_challenge_273_easy_getting_a_degree/


convert :: Double -> String -> String
convert n ('d':'r':xs) = show (n * pi / 180) ++ "r"
convert n ('r':'d':xs) = show (n * 180 / pi) ++ "d"
convert n ('c':'f':xs) = show (n * (9/5) + 32) ++ "f"
convert n ('c':'k':xs) = show (n + 273.15)     ++ "k"
convert n ('k':'c':xs) = show (n - 273.15)     ++ "c"
convert n ('k':'f':xs) = show ((n - 273.15) * (9/5) + 32) ++ "f"
convert n ('f':'c':xs) = show ((n - 32) * (5/9)) ++ "c"
convert n ('f':'k':xs) = show ((n - 32) * (5/9) + 273.15) ++ "k"
convert _ _            = "No candidate for conversion"

extract :: String -> (String, String)
extract = break (`elem` ['c'..'r'])

main :: IO ()
main = do
    input <- getLine
    let extracted = extract input
    putStrLn $ convert (read $ fst extracted) (snd extracted)
    main
