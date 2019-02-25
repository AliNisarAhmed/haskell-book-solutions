module Addition where

  import Test.Hspec
  import Test.QuickCheck

  sayHello :: IO ()
  sayHello = putStrLn "Hello!"

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where 
      go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1) 

  -- main :: IO ()
  -- main = hspec $ do
  --   describe "Addition" $ do
  --     it "1 + 1 is greater than 1" $ do
  --       (1 + 1) > 1 `shouldBe` True
  --     it "2 + 2 is equal to 4" $ do
  --       shouldBe (2 + 2) 4

  summation :: (Eq a, Num a) => a -> a -> a
  summation num1 1 = num1
  summation num1 num2 = num1 + summation num1 (num2 - 1)

  genBool :: Gen Bool
  genBool = choose (False, True)

  genBool' :: Gen Bool
  genBool' = elements [False, True]

  genOrdering :: Gen Ordering
  genOrdering = elements [LT, EQ, GT]

  genChar :: Gen Char
  genChar = elements ['a'..'z']

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
    frequency [(1, return Nothing), (3, return (Just a))] 

  prop_additionGreater :: Int -> Bool
  prop_additionGreater x = x + 0 > x

  runQc :: IO ()
  runQc = quickCheck prop_additionGreater

  main :: IO ()
  main = hspec $ do 
    describe "Summation" $ do
      it "5 multiplied by 3 is 15" $ do
        summation 5 3 `shouldBe` 15
      it "4 multiplied by 5 is 20" $ do
        summation 4 5 `shouldBe` 20
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)