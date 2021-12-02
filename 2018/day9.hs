import Data.Sequence

place_marble :: (Seq Int, Seq Int) -> Int -> Int -> (Seq Int, Seq Int)
place_marble (scores, marb_circ) marb_no player_no
    | marb_no > 0 && mod marb_no 23 == 0 =
        let backed_up = backup_marble_circ marb_circ 7
            torem = index backed_up 0
            remback = index (tails backed_up) 1
            score = torem + marb_no + (index scores player_no)
            new_scores = update player_no score scores
        in  (new_scores, remback)
    | otherwise = 
        let rot = forward_marble_circ marb_circ 2
        in  (scores, marb_no <| rot)
--place_marble (scores, (m1 : [])) marb_no _ = (scores, [marb_no, m1])
--place_marble (scores, []) marb_no _ = (scores, [marb_no])

backup_marble_circ :: Seq Int -> Int -> Seq Int
backup_marble_circ marb_circ count =
    let (st, end) = Data.Sequence.splitAt (Data.Sequence.length marb_circ - count) marb_circ
    in end >< st
        
-- TODO - make this and prev same...
forward_marble_circ :: Seq Int -> Int -> Seq Int
forward_marble_circ marb_circ count =
    let (st, end) = Data.Sequence.splitAt count marb_circ
    in end >< st

--put_show_space :: Show a => a -> IO ()
--put_show_space val = putStr ((show val) ++ " ")

main :: IO ()
main = do
    let player_count = 458
        marble_count = 7201900
        st_scores = fromList [0 | _ <- [0..(player_count - 1)]]
        (scores, _) = foldl (\a x -> place_marble a x (mod x player_count)) (st_scores, empty) [0..marble_count]
    --sequence_ $ map put_show_space marb_circ
    --putStrLn ""
    --sequence_ $ map put_show_space scores
    --putStrLn ""
    putStrLn $ show $ maximum scores
    return ()
