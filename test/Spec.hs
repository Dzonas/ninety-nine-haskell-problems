import Control.Exception (evaluate)
import NinetyNine
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NinetyNine" $ do
    describe "myLast" $ do
      it "myLast returns last element of the list of non-empty list" $ do
        myLast [1, 2, 3, 4] `shouldBe` (4 :: Int)

      it "myLast raises an error for empty list" $ do
        myLast [] `shouldThrow` errorCall "empty list"

    describe "myButLast" $ do
      it "myButLast returns second last element of a non-empty list" $ do
        myButLast [1, 2, 3, 4] `shouldBe` (3 :: Int)

      it "myButLast returns an error for an empty list" $ do
        evaluate (myButLast []) `shouldThrow` errorCall "list with less than 2 elements"

      it "myButLast returns an error for a list with one element" $ do
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
