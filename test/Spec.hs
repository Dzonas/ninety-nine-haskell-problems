import qualified MyLib (someFunc)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MyLib" $ do
    it "someFunc returns someFunc" $ do
      MyLib.someFunc `shouldBe` "someFunc"
