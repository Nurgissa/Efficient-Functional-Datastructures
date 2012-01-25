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
		  sTr ss   = Branch[(cp,sTr rss)| a <- alpha, let gs = select ss a, let (cp,rss) = edge_cst gs]