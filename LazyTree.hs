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

printTree :: (Show alf) => Int -> STree alf -> IO ()
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

                                                
--search :: (Eq alph) => Word alph -> STree alph -> Bool
--search w (Leaf)                  = False
--search w1 (Branch((w2, st):lst)) =  case g w1 ((w2, st):lst)) of 
--                                         ([],_) -> g w1 lst
--                                         (w2, v1:_) 
--                                    else if commonPrefix (w1:w2:[]) == w2 then search (stripPrefix w2 w1) st
--                                    else if commonPrefix (w1:w2:[]) == w1 then True
--                                    else False
--                                    where g w1 ((w2, st):lst)) = commonPrefix (w1:w2:[])
