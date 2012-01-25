module LazyTreeExt where

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
edge_cst [s] = (s, [[]])
edge_cst awss@((a:_):ss) | all (\(c:_) -> a == c) ss =  (a:cp, rss)
                         | otherwise               =  ([], awss)
                           where (cp, rss)         =  edge_cst ([t | _:t <- awss])

lazy_cst :: (Eq alph) => (EdgeFunction alph) -> Word alph -> Word alph -> STree alph
lazy_cst edge_cst alphabet t = sTr (suffixes t) 
    where sTr [[]] = Leaf 
          sTr ss   = Branch[(cp,sTr rss)| a <- alphabet, let gs = select ss a, let (cp,rss) = edge_cst gs]

is_same_firstLetter :: (Eq alph) => [Word alph] -> Bool
is_same_firstLetter [] = True
is_same_firstLetter [w] = True
is_same_firstLetter ((x:xs):(y:ys):xss) = (x==y) && (is_same_firstLetter ((x:xs):xss))

common_prefix :: (Eq alph) => [Word alph] -> Word alph

common_prefix ws = common_prefix_aux ws []
    where common_prefix_aux [] aux = reverse aux
          common_prefix_aux ws@((x:xs):xxs) aux | is_same_firstLetter ((x:xs):xxs) = common_prefix_aux ([ys | _:ys <- ws]) (x:aux)
                                                | otherwise                        = reverse aux

instance Show (STree alph) where
    show t = "" 

main = 
    let alphabet = ["a","b","c","d","e","f","g"]
        word = ["a","g","c","g","a","c","g","a","g"]
    in  lazy_cst edge_cst alphabet word
                                                
--search :: (Eq alph) => Word alph -> STree alph -> Bool
--search w Leaf = False
--search w1 (Branch[(w2, st):lst]) =  if common_prefix (w1:w2:[]) == [] then search w1 lst
--                                    else if common_prefix (w1:w2:[]) == w2 then search (stripPrefix w2 w1) st
--                                    else if common_prefix (w1:w2:[]) == w1 then True
--                                    else False