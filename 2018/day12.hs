import System.IO
import Data.List (sortOn)

strip :: String -> String
strip str = strip_left (reverse (strip_left (reverse str)))

strip_left :: String -> String
strip_left (s : remstr)
    | s == ' '  = strip_left remstr
    | s == '\n' = strip_left remstr
    | otherwise = s:remstr
strip_left [] = []

binform :: String -> Int
binform inval = sum [bv | (c, bv) <- zip inval [1,2,4,8,16], c == '#']

parse_rules :: String -> [Char]
parse_rules contents =
    let rule_lines = filter (not . null) $ drop 1 (lines contents)
        rules_binform = [(binform rule, out) | 
                            [rule, _, out] <- map words rule_lines]
    in  [out !! 0 | (_, out) <- sortOn fst rules_binform]
    
parse_init :: String -> String
parse_init contents = 
    let init_line = strip $ (lines contents) !! 0
    in  (words init_line) !! 2

patch_gen :: (String, Int) -> (String, Int)
patch_gen (curgen, firstind) = 
    let (revdpatchedback, _) = patch_front ((reverse curgen), firstind)
    in patch_front ((reverse revdpatchedback), firstind)

patch_front :: (String, Int) -> (String, Int)
patch_front (curgen, firstind)
    | take 5 curgen /= "....." = ("....." ++ curgen, firstind-5)
    | otherwise                = (curgen, firstind)

dogens :: [Char] -> Int -> (String, Int) -> (String, Int)
dogens rules gens (curgen, firstind)
    | gens == 0 = (curgen, firstind)
    | otherwise =
        let (patchedgen, newfirstind) = patch_gen (curgen, firstind)
            last_sub_st = (length patchedgen) - 5
            all_subs = [take 5 (drop n patchedgen) | n <- [0..last_sub_st]]
            bin_subs = [binform sub | sub <- all_subs]
            nextgen = ".." ++ [rules !! binval | binval <- bin_subs] ++ ".."
        in dogens rules (gens-1) (nextgen, newfirstind)

scoregen :: (String, Int) -> Int
scoregen (gen, firstind) = sum [ind | (c, ind) <- zip gen [firstind..], c == '#']

main :: IO ()
main = do
    handle <- openFile "input day 12.txt" ReadMode
    contents <- hGetContents handle
    let init_gen = parse_init contents
        rules = parse_rules contents
        doonegen = dogens rules 1
        gengen = scanl (\gen _ -> doonegen gen) (init_gen, 0) ([0..] :: [Int])
        (lastgen, firstind) = (drop 5000 gengen) !! 0
        scores = [scoregen t | t <- gengen]
        scorediff = [i-j | (i,j) <- zip (drop 1 scores) scores]
        show_scores = [putStrLn (show (score, diff)) | (score,diff) <- zip scores scorediff]
    --putStrLn lastgen
    --putStrLn $ show firstind
    putStrLn $ show $ scoregen (lastgen, firstind)
    --sequence_ $ take 20 show_scores
    return ()
