module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "evaluate ast" $
        evaluate (Multiply (Lit 4) (Add (Lit 2) (Lit 3))) `shouldBe` 20

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

