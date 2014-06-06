import Test.Hspec
import HsCmp
import Data.ByteString.Char8
import Data.Word8

main :: IO ()
main = hspec $ do
    describe "CmpResult" $ do

        it "Differ equals" $ Differ 1 1 `shouldBe` Differ 1 1
        it "Same equals" $ do Same `shouldBe` Same
        it "Differ doesn't equal" $ 
            Differ 1 2 `shouldSatisfy` (/= Differ 1 1)
        it "Same doesn't equal Differ" $
            Differ 1 2 `shouldSatisfy` (/= Same)

    describe "cmp'" $ do

        it "Returns Same" $ do
            cmp' 1 1 [_w] [_w] `shouldBe` Same

        it "Returns Differ" $ do
            cmp' 1 1 [_w] [_x] `shouldBe` Differ 1 1

            cmp' 1 1 [_w, _lf, _x] [_w, _lf, _w] `shouldBe` Differ 3 2
