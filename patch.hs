import Control.Applicative
import qualified Data.MemoCombinators as Memo

data Patch a
    = Keep a
    | Remove a
    | Add a
    deriving (Show)

--lcs :: (Eq a) => [a] -> [a] -> [[a]]
lcs = Memo.memo2 (Memo.list Memo.char) (Memo.list Memo.char) go
    where
    go [] _ = [[]]
    go _ [] = [[]]
    go (x:xs) (y:ys) 
        | x == y    = (x:) <$> lcs xs ys
        | otherwise = lcs (x:xs) ys <> lcs xs (y:ys)
    
    [] <> ys = ys
    xs <> [] = xs
    xs@(x:_) <> ys@(y:_) = 
        case compare (length x) (length y) of
            LT -> ys
            GT -> xs
            EQ -> xs ++ ys

-- lcsPatch lcs orig new
lcsPatch :: (Eq a) => [a] -> [a] -> [a] -> [Patch a]
lcsPatch (l:ls) (o:os) (n:ns)
    | l == o && o == n = Keep l   : lcsPatch ls os ns
    | l /= o           = Remove o : lcsPatch (l:ls) os (n:ns)
    | l /= n           = Add n    : lcsPatch (l:ls) (o:os) ns
lcsPatch [] os ns = map Remove os ++ map Add ns
lcsPatch _ _ _ = error "Impossible, lcs was not an lcs!"

applyPatch :: (Eq a) => [Patch a] -> [a] -> [a]
applyPatch (Keep x:ps) (x':xs) 
    | x == x' = x : applyPatch ps xs
    | otherwise = error "Patch was not a patch for this input when keeping"
applyPatch (Remove x:ps) (x':xs)
    | x == x' = applyPatch ps xs
    | otherwise = error "Patch was not a patch for this input when removing"
applyPatch (Add x:ps) xs = x : applyPatch ps xs
applyPatch [] [] = []
applyPatch [] _  = error "Input remaining when none expected"


applyPatch2 :: (Eq a) => [Patch a] -> [Patch a] -> [a] -> [Either (a,a) a]
applyPatch2 (Add a:ps) (Add b:qs) xs = Left (a,b) : applyPatch2 ps qs xs
applyPatch2 (Add a:ps) qs xs = Right a : applyPatch2 ps qs xs
applyPatch2 ps (Add b:qs) xs = Right b : applyPatch2 ps qs xs
applyPatch2 (Keep a:ps) (Keep b:qs) (x:xs)
    | a == b && b == x = Right x : applyPatch2 ps qs xs
applyPatch2 (Keep a:ps) (Remove b:qs) (x:xs)
    | a == b && b == x = applyPatch2 ps qs xs
applyPatch2 (Remove a:ps) (Keep b:qs) (x:xs)
    | a == b && b == x = applyPatch2 ps qs xs
applyPatch2 (Remove a:ps) (Remove b:qs) (x:xs)
    | a == b && b == x = applyPatch2 ps qs xs
applyPatch2 [] [] [] = []
