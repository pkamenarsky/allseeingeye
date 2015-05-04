module Match where

import Control.Monad

set1 :: Int -> a -> [a] -> [a]
set1 i x xs = take i xs ++ [x] ++ drop (i + 1) xs

set :: Int -> Int -> a -> [[a]] -> [[a]]
set x y v vs = set1 x (set1 y v (vs !! x)) vs

walk :: Eq a => [a] -> [a] -> [[Int]]
walk a b = foldl (\a (i, j) -> loop i j a) f [ (i, j) | i <- [1..length a - 1], j <- [1..length b - 1]]
  where
    f = take (length a) $ repeat (take (length b) $ repeat 0)

    loop i j f = set i j (max match (max insert delete)) f
      where
        match = f !! (i - 1) !! (j - 1) + sim (a !! i) (b !! j)
        delete = f !! (i - 1) !! j
        insert = f !! i !! (j - 1)

sim :: Eq a => a -> a -> Int
sim a b | a == b = 1
        | otherwise = -1

align :: (Eq a, Show a) => [a] -> [a] -> [[Int]] -> (String, String)
align a b f = go (length a - 1) (length b - 1) ("", "")
  where
    go 0 0 (ala, alb) = (ala, alb)
    go i j (ala, alb)
      | i > 0 && j > 0 && (f !! i !! j) == (f !! (i - 1) !! (j - 1)) + sim (a !! i) (b !! j)
        = go (i - 1) (j - 1) (show (a !! i) ++ ala, show (b !! j) ++ alb)
      | i > 0 && (f !! i !! j) == (f !! (i - 1) !! j)
        = go (i - 1) j (show (a !! i) ++ ala, "-" ++ alb)
      | otherwise
        = go i (j - 1) ("-" ++ ala, show (b !! j) ++ alb)

align2 :: (Eq a, Show a) => [a] -> [a] -> [[Int]] -> String
align2 a b f = go (length a - 1) (length b - 1) ""
  where
    go 0 0 al = al
    go i j al
      | i > 0 && j > 0 && (f !! i !! j) == (f !! (i - 1) !! (j - 1)) + sim (a !! i) (b !! j)
        = go (i - 1) (j - 1) (show (a !! i) ++ al)
      | i > 0 && (f !! i !! j) == (f !! (i - 1) !! j)
        = go (i - 1) j ("-" ++ show (a !! i) ++ al)
      | otherwise
        = go i (j - 1) ("+" ++ show (b !! j) ++ al)

nw :: (Eq a, Show a) => [a] -> [a] -> (String, String)
nw a b = align a b (walk a b)

nw2 :: (Eq a, Show a) => [a] -> [a] -> String
nw2 a b = align2 a b (walk a b)
