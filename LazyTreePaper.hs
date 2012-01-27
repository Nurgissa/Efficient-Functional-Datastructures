module LazyTreePaper where

data STree alf = Leaf
               | Branch[(Label alf, STree alf)]
               deriving Eq
type Label alf = ([alf], Int)
type EdgeFunction alf = [[alf]] -> (Int, [[alf]])

lazyTree :: (Eq alf) => (EdgeFunction alf) -> [alf] -> [alf] -> STree alf
lazyTree edge alpha t = sTr (suffixes t)
    where sTr [[]] = Leaf
          sTr ss   = Branch [((a:sa, 1 + cpl), sTr ssr) | a <- alpha,
                                                          sa:ssa <- [select ss a],
                                                          (cpl, ssr) <- [edge (sa:ssa)]]

select :: (Eq alf) => [[alf]] -> alf -> [[alf]]
select ss a = [u | c:u <- ss , a == c]

suffixes :: [alf] -> [[alf]]
suffixes []       = []              
suffixes aw@(_:w) = aw:suffixes w

lazy_cst :: (Eq alf) => [alf] -> [alf] -> STree alf
lazy_cst = lazyTree edge_cst

edge_cst :: (Eq alf) => EdgeFunction alf
edge_cst [s] = (length s, [[]])
edge_cst awss@((a:w):ss)
    | [] == [0 | c:_ <- ss, a /= c] = (1 + cpl, rss)
    | otherwise                  = (0, awss)
        where (cpl, rss) = edge_cst (w:[u | _:u <- ss])

printTree :: (Show alf) => Int -> STree alf -> IO ()
printTree i Leaf        = return ()
printTree i (Branch es) = g i es
    where
        g i ([])                = return ()
        g i (((w, pl), st):lst) = do putStr $ replicate i ' ' ++ "+"
                                     print $ take pl w
                                     printTree (i + 1) st
                                     g i lst

main = 
    let alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
        word = "agcgacgag"
    in  printTree 0 $ lazy_cst alphabet word