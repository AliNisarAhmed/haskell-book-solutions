module Addition where

  import Test.Hspec
  import Test.QuickCheck

  -- sayHello :: IO ()
  -- sayHello = putStrLn "Hello!"

  main :: IO ()
  main = hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do
        (1 + 1) > 1 `shouldBe` True
      it "2 + 2 == 4" $ do
        2 + 2 `shouldBe` 4
      it "x + 1 is always greater than x" $ do 
        property $ \x -> x + 1 > (x :: Int)

  genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
  genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

  genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
  genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

  genMaybe :: Arbitrary a => Gen (Maybe a)
  genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]
  
  genMaybe' :: Arbitrary a => Gen (Maybe a) 
  genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a)) ]