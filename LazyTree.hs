module LazyTree where

import Data.List

data STree alph = Leaf | Branch [(Word alph,STree alph)] deriving Eq

type Word alph = [alph]

type EdgeFunction alph = [Word alph] -> (Word alph,[Word alph])

suffixes :: (Word alph) -> [Word alph]
suffixes []       = []              
suffixes aw@(_:w) = aw:suffixes w 

select :: (Eq alph) => [Word alph] -> alph -> [Word alph]
select ss a = [u | c:u <- ss, length u > 0, a == c]

edge_cst :: (Eq alph) => EdgeFunction alph
edge_cst []     = ([], [])
edge_cst [s]    = (s, [[]])
edge_cst ([]:w) = ([], w)
edge_cst awss@((a:_):ss)
    | all p ss  = (a:cp, rss)
    | otherwise = ([], awss)
        where p []    = False
              p (c:_) = a == c
              (cp, rss) = edge_cst ([t | _:t <- awss])

lazy_cst :: (Eq alph) => (EdgeFunction alph) -> Word alph -> Word alph -> STree alph
lazy_cst edge_cst alphabet t = sTr (suffixes t) 
    where sTr [[]] = Leaf 
          sTr ss   = Branch[(a:cp, sTr rss)| a <- alphabet, let gs = select ss a, length gs > 0, let (cp, rss) = edge_cst gs]

printTree :: (Show alph) => Int -> STree alph -> IO ()
printTree i Leaf        = return ()
printTree i (Branch es) = g i es
    where
        g i ([])          = return ()
        g i ((w, st):lst) = do putStr $ replicate i ' ' ++ "+"
                               print w
                               printTree (i + 1) st
                               g i lst

main = 
    let alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
        word = "agcgacgag"
    in  printTree 0 $ lazy_cst edge_cst alphabet word

commonPrefix :: (Eq alph) => Word alph -> Word alph -> (Word alph, Word alph, Word alph)
commonPrefix w1 w2 = commonPrefix_aux w1 w2 []
                     where commonPrefix_aux [] w2 acc           = ([], w2, reverse acc)
                           commonPrefix_aux w1 [] acc           = (w1, [], reverse acc)
                           commonPrefix_aux (x:xs) (y:ys) acc
                                                   | (x == y)   = commonPrefix_aux xs ys (x:acc)
                                                   | otherwise  = (x:xs, y:ys, reverse acc)

search :: (Eq alph) => Word alph -> STree alph -> Bool
search w (Leaf)                  = False
search w1 (Branch((w2, st):lst)) =  g w1 (((w2, st):lst))
                                    where g w1 (((w2, st):lst)) = case commonPrefix w1 w2 of
                                                                       (w1, w2, []) -> g w1 lst
                                                                       (v1, [], w2) -> search v1 st
                                                                       ([], v2, w1) -> True
                                                                       (v1, v2, cp) -> False