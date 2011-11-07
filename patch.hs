{-# LANGUAGE PatternGuards #-}

import Data.List (nub)
import Control.Applicative
import qualified Data.MemoCombinators as Memo
import Debug.Trace
import qualified Data.Array as A

data Patch a
    = Keep a
    | Remove a
    | Add a
    deriving (Show)

lcs :: (Eq a) => [a] -> [a] -> [[a]]
lcs xs ys = snd $ table A.! snd (A.bounds table)
    where
    table = A.array ((0,0), (length xs-1, length ys-1)) 
            [ ((i,j), ((x, y), go i j)) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..] ]

    go 0 _ = [[]]
    go _ 0 = [[]]
    go i j
        | ((x,y),_) <- table A.! (i,j), x == y 
            = (x:) <$> snd (table A.! (i-1, j-1))
        | otherwise 
            = snd (table A.! (i-1,j)) <> snd (table A.! (i,j-1))
    
    [] <> ys = ys
    xs <> [] = xs
    xs@(x:_) <> ys@(y:_) = 
        case compare (length x) (length y) of
            LT -> ys
            GT -> xs
            EQ -> nub $ xs ++ ys

-- lcsPatch lcs orig new
lcsPatch :: (Eq a) => [a] -> [a] -> [a] -> [Patch a]
lcsPatch (l:ls) (o:os) (n:ns)
    | l == o && o == n = Keep l   : lcsPatch ls os ns
    | l /= o           = Remove o : lcsPatch (l:ls) os (n:ns)
    | l /= n           = Add n    : lcsPatch (l:ls) (o:os) ns
lcsPatch [] os ns = map Remove os ++ map Add ns
lcsPatch _ _ _ = error "Impossible, lcs was not an lcs!"

diff :: (Eq a) => [a] -> [a] -> [[Patch a]]
diff orig new = map (\l -> lcsPatch l orig new) $ lcs orig new

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
--applyPatch2 ps qs xs | trace (showDiff ps ++ "\n" ++ showDiff qs ++ "\n--------\n" ++ xs ++ "\n") False = undefined
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

merge3 :: (Eq a) => [a] -> [a] -> [a] -> [[Either (a,a) a]]
merge3 orig left right = liftA2 applyPatch2 (diff orig left) (diff orig right) <*> pure orig

fromResult :: [Either (Char,Char) Char] -> String
fromResult = (f =<<)
    where
    f (Left (x,y)) = "[" ++ [x] ++ "|" ++ [y] ++ "]"
    f (Right x) = [x]

showDiff :: [Patch Char] -> String
showDiff = (f =<<)
    where
    f (Keep x) = " " ++ [x]
    f (Remove x) = "-" ++ [x]
    f (Add x) = "+" ++ [x]
