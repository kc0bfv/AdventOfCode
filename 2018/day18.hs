import Data.Array

strip_ws :: String -> String
strip_ws (val : rest)
    | (val == ' ') or (val == '\n') = strip_ws rest
    | otherwise = val : strip_ws rest
strip_ws [] = []

main :: IO ()
main = do
    handle <- openFile "input day 18.txt" ReadMode
    contents <- hGetContents handle
    let
        stripped_lines = lines $ strip_ws contents
        width = length (stripped_lines !! 0)
        height = length stripped_lines
        inarr = array ((0,0), (width-1, height-1)) 
            [((x, y), (stripped_lines !! y) !! x) | x <- [0..width-1], y <- [0..height-1]]

    putStrLn $ show inarr
   
