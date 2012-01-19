module LazyTreeExt where

import Data.List

-- | Suffix tree representation
data STree alf = Leaf | Branch [(Label alf, STree alf)] deriving Eq

-- | Suffix tree label representation
type Label alf = [alf]

-- | Type of edge functions
type EdgeFunction alf = [[alf]] -> ([alf], [[alf]])

lazy_cst :: (Eq alf) => [alf] -> [alf] -> STree alf
lazy_cst = lazyTree edge_cst

-- | Builds a suffix tree for the word @t@.
lazyTree :: (Eq alf) => (EdgeFunction alf) -> [alf] -> [alf] -> STree alf
lazyTree edge alpha t = sTr (suffixes t)
    where sTr [[]] = Leaf
          sTr ss   = Branch [(cp, sTr ssr) | a <- alpha,
                                             let gs = select ss a,
                                             let (cp, ssr) = edge gs]

-- | Returns a list with all words of @ss@, which starts with the letter @a@.
select :: (Eq alf) => [[alf]] -> alf -> [[alf]]
select ss a = [c:u | c:u <- ss, a == c]

-- | Returns a list with all suffixes of the given word.
suffixes :: [alf] -> [[alf]]
suffixes []       = []
suffixes aw@(_:w) = aw:suffixes w

-- | edge function for building a cst.
edge_cst :: (Eq alf) => EdgeFunction alf
edge_cst [s] = ([s], [[]])
edge_cst awss@((a:_):ss) | all (\c:_ -> a == c) ss = (a:cp, rss)
                         | otherwise               = ([], awss)
                           where (cp, rss) = edge_cst ([t | _:t <- awss])