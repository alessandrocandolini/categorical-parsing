module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "evaluate ast" $
        evaluate 4 * (3 + 2) `shouldBe` 20

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

