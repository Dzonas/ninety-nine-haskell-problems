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
