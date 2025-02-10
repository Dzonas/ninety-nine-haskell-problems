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
