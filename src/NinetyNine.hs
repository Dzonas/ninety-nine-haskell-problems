module NinetyNine where

import Data.List (uncons)
import Data.Maybe (fromJust)
import System.Random hiding (split)

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

-- Ex.12
decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = x : decodeModified xs
decodeModified ((Multiple n x) : xs) = replicate n x ++ decodeModified xs

-- Ex.13
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x : xs) = case e of
  Single x' -> if x == x' then Multiple 2 x : es else [Single x, Single x'] ++ es
  Multiple n x' -> if x == x' then Multiple (n + 1) x : es else [Single x, Multiple n x'] ++ es
  where
    (e, es) = fromJust . uncons . encodeDirect $ xs

-- Ex.14
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- Ex.15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = repli' x n ++ repli xs n

repli' :: a -> Int -> [a]
repli' _ 0 = []
repli' x n = x : repli' x (n - 1)

-- Ex.16
dropEvery :: [a] -> Int -> [a]
dropEvery xs 0 = xs
dropEvery xs n = dropEvery' xs n n

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (_ : xs) n 1 = dropEvery' xs n n
dropEvery' (x : xs) n n' = x : dropEvery' xs n (n' - 1)

-- Ex.17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x : xs) n
  | n == 0 = ([], x : xs)
  | otherwise = (x : left, right)
  where
    (left, right) = split xs (n - 1)

-- Ex.18
slice :: [a] -> Int -> Int -> [a]
slice xs i k
  | k < i = error "right index is less than left index"
  | otherwise = slice' xs i k

slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' (x : xs) i k
  | i > 1 = slice' xs (i - 1) (k - 1)
  | k > 0 = x : slice' xs i (k - 1)
  | otherwise = []

-- Ex.19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n > 0 =
      let n' = mod n (length xs)
          (left, right) = splitAt n' xs
       in right ++ left
  | n < 0 =
      let n' = length xs - abs n
          (left, right) = splitAt n' xs
       in right ++ left
  | otherwise = xs

-- Ex.20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = case splitAt (n - 1) xs of
  (left, r : rs) -> (r, left ++ rs)
  _ -> error "index out of bounds"

-- Ex.21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = left ++ [x] ++ right
  where
    (left, right) = splitAt (n - 1) xs

-- Ex.22
range :: Int -> Int -> [Int]
range a b = [a .. b]

-- Ex.23
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n = do
  gen <- getStdGen
  return $ take n [xs !! x | x <- randomRs (0, length xs - 1) gen]
