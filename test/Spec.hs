import qualified NinetyNine (someFunc)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NinetyNine" $ do
    it "someFunc returns someFunc" $ do
      NinetyNine.someFunc `shouldBe` "someFunc"
