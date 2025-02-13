import Control.Exception (evaluate)
import NinetyNine
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
