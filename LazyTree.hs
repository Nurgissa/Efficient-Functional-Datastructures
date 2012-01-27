module LazyTree where

import Data.List

data STree alph = Leaf | Branch [(Word alph,STree alph)] deriving Eq

type Word alph = [alph]

type EdgeFunction alph = [Word alph] -> (Word alph,[Word alph])

suffixes :: (Word alph) -> [Word alph]
suffixes []       = []              
suffixes aw@(_:w) = aw:suffixes w 

select :: (Eq alph) => [Word alph] -> alph -> [Word alph]
select ss a = [c:u | c:u <- ss, a == c]

edge_cst :: (Eq alph) => EdgeFunction alph
edge_cst [s]    = (s, [[]])
edge_cst awss@((a:_):ss)
    | all (\(c:_) -> a == c) ss  = (a:cp, rss)
    | otherwise                  = ([], awss)
        where (cp, rss) = edge_cst ([t | _:t <- awss, length t > 0])

lazy_cst :: (Eq alph) => (EdgeFunction alph) -> Word alph -> Word alph -> STree alph
lazy_cst edge_cst alphabet t = sTr (suffixes t) 
    where sTr [[]] = Leaf 
          sTr ss   = Branch[(cp, sTr rss)| a <- alphabet, let gs = select ss a, length gs > 0, let (cp, rss) = edge_cst gs]

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
    in  search "gag" $ lazy_cst edge_cst alphabet word

commonPrefix :: (Eq alph) => Word alph -> Word alph -> (Word alph, Word alph, Word alph)
commonPrefix w1 w2 = commonPrefix_aux w1 w2 []
                     where commonPrefix_aux (x:xs) (y:ys) cp | (x == y) = commonPrefix_aux xs ys (x:cp)
                           commonPrefix_aux  w1     w2    cp            = (w1, w2, reverse cp)

search :: (Eq alph) => Word alph -> STree alph -> Bool
search ss Leaf                = False
search ss (Branch es) =  g ss es
    where g ss (((l, st):es)) = case commonPrefix ss l of
                                  ([], _, _)   -> True
                                  (_, _, [])   -> g ss es
                                  (ss', [], _) -> search ss' st
                                  _            -> False
