import Control.Exception (evaluate)
import NinetyNine
import System.Random (mkStdGen, setStdGen)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NinetyNine" $ do
    describe "myLast" $ do
      it "returns last element of a non-empty list" $ do
        myLast [1, 2, 3, 4] `shouldBe` (4 :: Int)

      it "raises an error for an empty list" $ do
        myLast [] `shouldThrow` errorCall "empty list"

    describe "myButLast" $ do
      it "returns second last element of a non-empty list" $ do
        myButLast [1, 2, 3, 4] `shouldBe` (3 :: Int)

      it "returns an error for an empty list" $ do
        evaluate (myButLast []) `shouldThrow` errorCall "list with less than 2 elements"

      it "returns an error for a list with one element" $ do
        evaluate (myButLast [1 :: Int]) `shouldThrow` errorCall "list with less than 2 elements"

    describe "elementAt" $ do
      it "returns element at n-th position starting with 1" $ do
        elementAt [1, 2, 3] 2 `shouldBe` (2 :: Int)

      it "throws an error when given 0 as index" $ do
        evaluate (elementAt [1 :: Int, 2, 3] 0) `shouldThrow` errorCall "index outside of bounds"

      it "throws an error when given negative index" $ do
        evaluate (elementAt ([1, 2, 3] :: [Int]) (-1)) `shouldThrow` errorCall "index outside of bounds"

      it "throws an error when given index outside of bounds" $ do
        evaluate (elementAt ([1, 2, 3] :: [Int]) 4) `shouldThrow` errorCall "index outside of bounds"

    describe "myLength" $ do
      it "returns length of a non-empty list" $ do
        myLength ([1, 2, 3] :: [Int]) `shouldBe` (3 :: Int)

      it "returns 0 when given an empty list" $ do
        myLength [] `shouldBe` (0 :: Int)

    describe "myReverse" $ do
      it "returns reverse of a non-empty list" $ do
        myReverse [1, 2, 3] `shouldBe` ([3, 2, 1] :: [Int])

      it "return empty list when given empty list" $ do
        myReverse [] `shouldBe` ([] :: [Int])

    describe "isPalindrome" $ do
      it "returns False when given non palindrome" $ do
        isPalindrome ([1, 2, 3] :: [Int]) `shouldBe` False

      it "returns True when given palindrome of even length" $ do
        isPalindrome "abba" `shouldBe` True

      it "returns True when given palindrome of odd length" $ do
        isPalindrome "kayak" `shouldBe` True

    describe "flatten" $ do
      it "returns list with one value if given Elem" $ do
        flatten (Elem 5) `shouldBe` ([5] :: [Int])

      it "returns empty list when given List []" $ do
        flatten (List []) `shouldBe` ([] :: [Int])

      it "returns flat list containing all elements of NestedList" $ do
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1, 2, 3, 4, 5] :: [Int])

    describe "compress" $ do
      it "returns empty list when given empty list" $ do
        compress [] `shouldBe` ([] :: [Int])

      it "returns list unchanged when given list with one element" $ do
        compress [1] `shouldBe` ([1] :: [Int])

      it "returns list with consecutive duplicates removed" $ do
        compress "aaaabccaadeeee" `shouldBe` "abcade"

    describe "pack" $ do
      it "returns an empty list when given an empty list" $ do
        pack [] `shouldBe` ([] :: [[Int]])

      it "returns a list containing one list with one element when given a list with one element" $ do
        pack [5] `shouldBe` ([[5]] :: [[Int]])

      it "returns a list of lists where consecutive duplicates are in the same sublists" $ do
        pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

    describe "encode" $ do
      it "returns an empty list when given an empty list" $ do
        encode [] `shouldBe` ([] :: [(Int, Int)])

      it "returns run-length encoding of given list" $ do
        encode "aaaabccaadeeee" `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]

    describe "encodeModified" $ do
      it "returns an empty list when given an empty list" $ do
        encodeModified [] `shouldBe` ([] :: [Encoded Int])

      it "returns run-length encoding of a given list using Encoded type" $ do
        encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

    describe "decodeModified" $ do
      it "returns an empty list when given an empty list" $ do
        decodeModified [] `shouldBe` ([] :: [Int])

      it "returns decoded run-length encoding" $ do
        decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

    describe "encodeDirect" $ do
      it "returns an empty list when given an empty list" $ do
        encodeDirect [] `shouldBe` ([] :: [Encoded Int])

      it "returns a list with one element of Single a when given a list with single element" $ do
        encodeDirect "a" `shouldBe` [Single 'a']

      it "returns run-length encoding of a given list using Encoded type" $ do
        encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

    describe "dupli" $ do
      it "returns an empty list when given an empty list" $ do
        dupli [] `shouldBe` ([] :: [Int])

      it "returns a list with every element duplicated" $ do
        dupli [1, 2, 3] `shouldBe` ([1, 1, 2, 2, 3, 3] :: [Int])

    describe "repli" $ do
      it "returns an empty list when given an empty list and n > 0" $ do
        repli [] 5 `shouldBe` ([] :: [Int])

      it "returns an empty list when given an empty list and n == 0" $ do
        repli [] 0 `shouldBe` ([] :: [Int])

      it "returns an empty list when given an non-empty list and n == 0" $ do
        repli "abc" 0 `shouldBe` ""

      it "returns a list with every element repeated n times" $ do
        repli "abc" 3 `shouldBe` "aaabbbccc"

    describe "dropEvery" $ do
      it "returns an empty list when given an empty list" $ do
        dropEvery "" 5 `shouldBe` ""

      it "returns original list unchanged when n == 0" $ do
        dropEvery "abcdefghik" 0 `shouldBe` "abcdefghik"

      it "returns a list with every n-th element dropped" $ do
        dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    describe "split" $ do
      it "returns a tuple of empty lists when given an empty list" $ do
        split [] 10 `shouldBe` (([], []) :: ([Int], [Int]))

      it "returns a tuple of two lists where the first list has all the elements of the original when index is after the end of the list" $ do
        split "abc" 10 `shouldBe` ("abc", "")

      it "returns a tuple of two lists where the second list has all the elements of the original when given index == 0" $ do
        split "abc" 0 `shouldBe` ("", "abc")

      it "returns a tuple of two lists, where the first list has all elements before given index and the second has the rest" $ do
        split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    describe "slice" $ do
      it "returns an empty list when given an empty list" $ do
        slice [] 1 2 `shouldBe` ([] :: [Int])

      it "returns an empty list when indexes are out of bounds" $ do
        slice "ab" 3 4 `shouldBe` ""

      it "returns a slice from 'i' to the end of the list if 'k' is outside of bounds" $ do
        slice "abc" 2 10 `shouldBe` "bc"

      it "returns the same element if i == k" $ do
        slice "abc" 2 2 `shouldBe` "b"

      it "returns all elements between 'i' and 'k' if both are inside of bounds" $ do
        slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7 `shouldBe` "cdefg"

      it "throws an error when k < i" $ do
        evaluate (slice "abc" 5 2) `shouldThrow` errorCall "right index is less than left index"

    describe "rotate" $ do
      it "returns an empty list when given an empty list" $ do
        rotate "" 10 `shouldBe` ""

      it "returns the original list unchanged when n == 0" $ do
        rotate "abc" 0 `shouldBe` "abc"

      it "returns list rotated left when given a positive n" $ do
        rotate "abcdefgh" 3 `shouldBe` "defghabc"

      it "returns list rotated right when given a negative n" $ do
        rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"

      it "return list rotated by modulo of n" $ do
        rotate "abcdefgh" 9 `shouldBe` "bcdefgha"

    describe "removeAt" $ do
      it "throws an error when given an empty list" $ do
        evaluate (removeAt 2 "") `shouldThrow` errorCall "index out of bounds"

      it "throws an error when given an negative index" $ do
        evaluate (removeAt (-1) "") `shouldThrow` errorCall "index out of bounds"

      it "throws an error when given an index after the end of the list" $ do
        evaluate (removeAt 5 "abcd") `shouldThrow` errorCall "index out of bounds"

      it "returns an element at given index and a list without that element" $ do
        removeAt 2 "abcd" `shouldBe` ('b', "acd")

    describe "insertAt" $ do
      it "returns 1 element list when given an empty list and any index" $ do
        insertAt 5 [] (-1) `shouldBe` ([5] :: [Int])

      it "returns list with an element inserted" $ do
        insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    describe "range" $ do
      it "returns an empty list when b is less than a" $ do
        range 1 (-2) `shouldBe` []

      it "returns a one element list when a == b" $ do
        range 1 1 `shouldBe` [1]

      it "returns a list with elements from a to b" $ do
        range 4 9 `shouldBe` [4, 5, 6, 7, 8, 9]

    describe "rndSelect" $ do
      it "returns an empty list when given an empty list" $ do
        result <- rndSelect "" 3
        result `shouldBe` ""

      it "returns random elements from a list" $ do
        setStdGen (mkStdGen 42)
        result <- rndSelect "abcdefgh" 3
        result `shouldBe` "ahh"

    describe "diffSelect" $ do
      it "returns a one element list when n == 1" $ do
        setStdGen (mkStdGen 42)
        result <- diffSelect 1 49
        result `shouldBe` [49]

      it "returns n different random numbers from set 1..m" $ do
        setStdGen (mkStdGen 42)
        result <- diffSelect 6 49
        result `shouldBe` [21, 18, 16, 33, 24, 49]

      it "throws an error when m < 1" $ do
        (diffSelect 6 0 >>= evaluate) `shouldThrow` errorCall "m < 1"

      it "returns an empty list when given n = 0" $ do
        result <- diffSelect 0 49
        result `shouldBe` []

      it "returns an empty list when given n < 0" $ do
        result <- diffSelect (-1) 49
        result `shouldBe` []
