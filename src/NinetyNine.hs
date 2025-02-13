module NinetyNine where

import Data.List (uncons)
import Data.Maybe (fromJust)

-- Ex.1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Ex.2
myButLast :: [a] -> a
myButLast [] = error "list with less than 2 elements"
myButLast [_] = error "list with less than 2 elements"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Ex.3
elementAt :: [a] -> Int -> a
elementAt xs = elementAt' xs 1

elementAt' :: [a] -> Int -> Int -> a
elementAt' [] _ _ = error "index outside of bounds"
elementAt' (x : xs) k n
  | k == n = x
  | otherwise = elementAt' xs (k + 1) n

-- Ex.4
myLength :: [a] -> Int
myLength = sum . map (const 1)

-- Ex.5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Ex.6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Ex.7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem n) = [n]
flatten (List []) = []
flatten (List (x : xs)) = flatten x <> flatten (List xs)

-- Ex.8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : xs)
  | x == x' = x' : xs'
  | otherwise = x : x' : xs'
  where
    (x', xs') = fromJust . uncons . compress $ xs

-- Ex.9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : xs)
  | x == x' = (x : xs') : xss
  | otherwise = [x] : xs' : xss
  where
    (x', _) = fromJust . uncons $ xs'
    (xs', xss) = fromJust . uncons . pack $ xs

-- Ex.10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\xs' -> (length xs', head' xs')) . pack $ xs
  where
    head' = fst . fromJust . uncons

-- Ex.11
data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map encodeModified' . encode

encodeModified' :: (Int, a) -> Encoded a
encodeModified' (1, x) = Single x
encodeModified' (n, x) = Multiple n x
