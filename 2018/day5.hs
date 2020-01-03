import Data.Char

elim_dupes :: String -> String
elim_dupes dat =
    let runres = elim_run dat
    in
        if runres == dat then dat
        else elim_dupes runres

elim_run :: String -> String
elim_run (d1 : d2 : remdat)
    | (abs (ord d1 - ord d2)) == 32 = elim_run remdat
    | otherwise = (d1 : (elim_run (d2 : remdat)))
elim_run [d1] = [d1]
elim_run [] = []

elim_chr :: Char -> String -> String
elim_chr ent (d1 : remdat)
    | ent == d1 = elim_chr ent remdat
    | (abs (ord d1 - ord ent)) == 32 = elim_chr ent remdat
    | otherwise = d1 : (elim_chr ent remdat)
elim_chr _ [] = []

main :: IO ()
main = do
    dat <- getLine
    let elimeddat = elim_dupes dat
        elimelimchr = [elim_dupes (elim_chr c elimeddat) | c <- ['a' .. 'z']]
        elimedlens = [length s | s <- elimelimchr]
    putStrLn ("Part 1: " ++ (show (length elimeddat)))
    putStrLn ("Part 2: " ++ (show (minimum elimedlens)))
    
