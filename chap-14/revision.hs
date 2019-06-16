module Revision where

  import Test.Hspec
  import Recursion

  main :: IO ()
  main = hspec $ do 
    describe "digitToWord" $ do 
      it "returns zero for 0" $ do 
        digitToWord 0 `shouldBe` "zero"