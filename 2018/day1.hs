import Text.Read (read)
import Text.Show (show)
import Data.Set (insert, member, empty)

import Debug.Trace (trace)

input_line_list = do
  line <- getLine
  if null line then return []
  else do
    followlist <- input_line_list
    return (line : followlist)

int_conv val
    | (head val) == '+' = (read (tail val) :: Int)
    | otherwise = (read val :: Int)

get_first_dupe_sum lines = get_first_dupe_sum_recur lines empty 0
--get_first_dupe_sum_recur lines prev tot | trace ("rfds " ++ (show tot)) False = undefined
get_first_dupe_sum_recur lines prev tot =
    let newtot = tot + (head lines)
    in
        if (member newtot prev) then newtot
        else get_first_dupe_sum_recur (tail lines) (insert newtot prev) newtot

inf_list list = inf_list_repeat list list
inf_list_repeat list orig_list
    | (tail list) == [] = (head list) : inf_list_repeat orig_list orig_list
    | otherwise         = (head list) : inf_list_repeat (tail list) orig_list

main = do
    lines <- input_line_list
    let int_lines = map int_conv lines
    putStrLn $ ("Part 1: " ++ (show $ sum int_lines))
    putStrLn $ ("Part 2: " ++ (show $ get_first_dupe_sum (inf_list int_lines)))
