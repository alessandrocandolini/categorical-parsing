module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "evaluate ast" $
        evaluate 4 * (3 + 2) `shouldBe` 20

     it "parse literal" $
        (parse "3") `shouldBe` Just 3

     it "parse valid (simple add case)" $
        (parse "(3+2)") `shouldBe` Just (3+2)

     it "parse valid (simple multiply case)" $
        (parse "(3*2)") `shouldBe` Just (3*2)

     it "parse valid (nested case)" $
        (parse "(4*(3+2))") `shouldBe` Just (4 * (3+2))

     it "parse valid (nested case with spaces)" $
        (parse "(4 *(3 + 2))") `shouldBe` Just (4 * (3+2))

     it "evaluate parse valid (simple case)" $
        fmap evaluate (parse "(3+2)") `shouldBe` Just 5

     it "evaluate parse valid (simple case)" $
        fmap evaluate (parse "(4*(3+2))") `shouldBe` Just 20

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

