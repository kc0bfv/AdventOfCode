import System.IO
import Text.Read (read)
import Text.Show (show)

data Node = Node { get_children :: [Node], get_metadata :: [Int], get_value :: Int }

split :: Char -> String -> [String]
split split_chr (cur_chr:rem_lst)
    | cur_chr == split_chr = split split_chr rem_lst
    | otherwise = split_takeval split_chr [cur_chr] rem_lst
split _ [] = []

split_takeval :: Char -> String -> String -> [String]
split_takeval split_chr cur_str (cur_chr:rem_lst)
    | cur_chr == split_chr = cur_str : split split_chr rem_lst
    | otherwise = split_takeval split_chr (cur_str ++ [cur_chr]) rem_lst
split_takeval _ cur_str [] = [cur_str]

text_lst_to_int_lst :: [String] -> [Int]
text_lst_to_int_lst lst = map read lst

read_node :: [Int] -> (Node, [Int])
read_node (child_count : meta_count : chld_lst) = 
    let (childnodes, meta_lst) = read_children child_count chld_lst
    in  let (metadata, rem_lst) = read_metadata meta_count meta_lst
        in  let value = node_value childnodes metadata
            in (Node childnodes metadata value, rem_lst)
read_node (_ : []) = (Node [] [] (-1), [])
read_node [] = (Node [] [] (-1), [])

read_children :: Int -> [Int] -> ([Node], [Int])
read_children child_count chld_lst
    | child_count == 0 = ([], chld_lst)
    | otherwise = let (node, more_chld_lst) = read_node chld_lst
                  in let (other_children, rem_lst) = read_children (child_count - 1) more_chld_lst
                     in (node : other_children, rem_lst)
        
read_metadata :: Int -> [Int] -> ([Int], [Int])
read_metadata meta_count (meta_val:meta_rm) 
    | meta_count == 0 = ([], meta_val:meta_rm)
    | otherwise = let (meta_vals, rem_lst) = read_metadata (meta_count - 1) meta_rm
                  in  (meta_val:meta_vals, rem_lst)
read_metadata _ [] = ([], [])

node_value :: [Node] -> [Int] -> Int
node_value child_nodes metadata
    | length child_nodes == 0 = sum metadata
    | otherwise = sum (nv_ind_metadat child_nodes metadata)

nv_ind_metadat :: [Node] -> [Int] -> [Int]
nv_ind_metadat child_nodes (metadat:rem_md)
    | 0 <= (metadat - 1) && (metadat - 1) < (length child_nodes) = (get_value (child_nodes !! (metadat - 1))) : nv_ind_metadat child_nodes rem_md
    | otherwise = 0 : nv_ind_metadat child_nodes rem_md
nv_ind_metadat _ [] = []

all_metadata :: Node -> [Int]
all_metadata root =
    let child_metadat = [all_metadata child | child <- get_children root]
    in  (get_metadata root) ++ (concat_all child_metadat)

concat_all :: [[a]] -> [a]
concat_all (fst_lst:others) = fst_lst ++ (concat_all others)
concat_all [] = []

--print_list :: [String] -> IO ()
--print_list (cur_mem:rem_lst) = do
--    putStrLn cur_mem
--    print_list rem_lst
--print_list [] = return ()

main :: IO ()
main = do
    handle <- openFile "input day 8.txt" ReadMode
    contents <- hGetContents handle
    let split_res = split ' ' contents
        int_lst = text_lst_to_int_lst split_res
        (tree, _) = read_node int_lst
        topval = get_value tree
        metadat_sum = sum (all_metadata tree)
    putStrLn (show metadat_sum)
    putStrLn (show topval)
