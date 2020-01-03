import Data.Set (insert, member, empty)

input_line_list = do
    line <- getLine
    if null line then return []
    else do
        followlist <- input_line_list
        return (line : followlist)

count_inst chr str = count_inst_recur chr str 0
count_inst_recur chr str cur
    | str == []        = cur
    | chr == head str  = count_inst_recur chr (tail str) (cur + 1)
    | otherwise        = count_inst_recur chr (tail str) cur

has_n_dupes n str = has_n_dupes_recur n str empty
has_n_dupes_recur n str tried
    | str == [] = False
    | member (head str) tried = has_n_dupes_recur n (tail str) tried
    | count_inst (head str) str == n = True
    | otherwise = has_n_dupes_recur n (tail str) (insert (head str) tried)

bool_to_str val = if val then "True" else "False"

output_list lst = do
    putStrLn (head lst)
    if (tail lst) /= [] then output_list (tail lst)
    else return ()

output_tuple_list lst = do
    putStr (fst (head lst))
    putStrLn (snd (head lst))
    if (tail lst) /= [] then output_tuple_list (tail(lst))
    else return ()

count_str_diffs :: Eq a => [a] -> [a] -> Integer
count_str_diffs s1 s2 = count_str_diff_recur s1 s2 0

count_str_diff_recur :: Eq a => [a] -> [a] -> Integer -> Integer
count_str_diff_recur [] [] cnt = cnt
count_str_diff_recur s1 [] cnt = count_str_diff_recur (tail s1) [] (cnt + 1)
count_str_diff_recur [] s2 cnt = count_str_diff_recur [] (tail s2) (cnt + 1)
count_str_diff_recur s1 s2 cnt = count_str_diff_recur (tail s1) (tail s2) (
        if (head s1) == (head s2) then cnt
        else (cnt + 1)
        )

compare_str_diffs :: Eq a => [a] -> [[a]] -> [(Integer, [a])]
compare_str_diffs comp_val comp_with
    | comp_with == []  = []
    | otherwise = (count_str_diffs comp_val (head comp_with), (head comp_with)) : compare_str_diffs comp_val (tail comp_with)

find_one_diff :: Eq a => [[a]] -> [([(Integer,[a])], [a])]
find_one_diff lines
    | lines == [] = []
    | otherwise = 
        let cur = head lines
            found_diffs = compare_str_diffs cur (tail lines)
            filtered = filter (\x -> (fst x) == 1) found_diffs
        in (filtered, cur) : find_one_diff (tail lines)

output_filted filt_ent = do
    putStrLn (snd filt_ent)
    let str_lst = (snd (unzip (fst filt_ent)))
    output_list str_lst

main = do
    lines <- input_line_list
    let with_2 = count_inst True (map (\x -> has_n_dupes 2 x) lines)
        with_3 = count_inst True (map (\x -> has_n_dupes 3 x) lines)
    putStrLn ("Part 1 " ++ (show (with_2 * with_3)))
    let found = find_one_diff lines
        filtered = filter (\x -> (fst x) /= []) found
        map_out = map output_filted filtered
    putStrLn "Done"
