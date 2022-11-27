module ParserSpec where

import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "parse true" $
        run bool "True" `shouldBe` Just True

     it "parse false" $
        run bool "False" `shouldBe` Just False

     it "parse valid digit" $ 
        run digit "1" `shouldBe` Just '1'
     
     it "fail to parse empty string as digit" $ 
        run digit "" `shouldBe` Nothing 

     it "fail to parse strings with more than one char as digits" $ 
        run digit "12" `shouldBe` Nothing 

     it "int fails to parse empty string" $ 
        run int "" `shouldBe` Nothing 

     prop "parse any int" $
        \i -> i >= 0 ==> run int (show i) `shouldBe` Just i 

     it "fail to parse wrong int" $
        run int "a10b" `shouldBe` Nothing

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

