module NinetyNine where

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "list with less than 2 elements"
myButLast [_] = error "list with less than 2 elements"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt xs = elementAt' xs 1

elementAt' :: [a] -> Int -> Int -> a
elementAt' [] _ _ = error "index outside of bounds"
elementAt' (x : xs) k n
  | k == n = x
  | otherwise = elementAt' xs (k + 1) n

myLength :: [a] -> Int
myLength = sum . map (const 1)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
