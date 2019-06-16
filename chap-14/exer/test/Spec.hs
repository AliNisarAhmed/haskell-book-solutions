module Main where

  import Test.Hspec
  import Lib
  import Test.QuickCheck
  import Data.List (sort)


  -- main :: IO ()
  -- main = hspec $ do 
  --   describe "digitToWord" $ do 
  --     it "returns zero for 0" $ do 
  --       digitToWord 0 `shouldBe` "zero"
  --     it "returns one for 1" $ do 
  --       digitToWord 1 `shouldBe` "one"

  --   describe "digits" $ do 
  --     it "returns [1] for 1" $ do 
  --       digits 1 `shouldBe` [1]
  --     it "returns [1, 0, 0] for 100" $ do 
  --       digits 100 `shouldBe` [1, 0, 0]

  --   describe "wordNumber" $ do 
  --     it "one-zero-zero given 100" $ do 
  --       wordNumber 100
  --         `shouldBe` "one-zero-zero"
  --     it "nine-zero-zero-one for 9001" $ do 
  --       wordNumber 9001 `shouldBe` "nine-zero-zero-one"

  ----------------------
  half :: (Fractional a) => a -> a
  half x = x / 2

  halfIdentity :: (Fractional a) => a -> a
  halfIdentity = (*2) . half

  prop_half :: Double -> Bool
  prop_half x = halfIdentity x == x

  -- main :: IO ()
  -- main = quickCheck prop_half

  ---------------------------------

  listOrdered :: (Ord a) => [a] -> Bool
  listOrdered xs = 
    snd $ foldr go (Nothing, True) xs
      where 
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

  prop_listOrdered :: (Ord a) => [a] -> Bool
  prop_listOrdered xs = (listOrdered $ sort xs ) == True 

  -- testListOrdered :: IO ()
  -- testListOrdered = do 
  --   quickCheck (prop_listOrdered :: [Integer] -> Bool)
  --   quickCheck (prop_listOrdered :: [Char] -> Bool)

  -- main :: IO ()
  -- main = quickCheck (prop_listOrdered :: [Integer] -> Bool)

  --------------------

  plusAssociative x y z = 
    x + (y + z) == (x + y) + z

  plusCommutative x y = 
    x + y == y + x

  -- main :: IO ()
  -- main = quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  -- main = quickCheck (plusAssociative :: Float -> Float -> Float -> Bool)  // DOes not hold
  -- main = quickCheck (plusCommutative :: Float -> Float -> Bool)  // Holds
  -- main = quickCheck (plusCommutative :: Int -> Int -> Bool)

  nonZeroArbitrary :: (Num a, Eq a, Arbitrary a) => Gen a
  nonZeroArbitrary = arbitrary `suchThat` (/= 0)

  twoNonZeroes :: Gen (Integer, Integer)
  twoNonZeroes = do 
    a <- nonZeroArbitrary
    b <- nonZeroArbitrary
    return (a, b)

  quotLaw :: Integral a => a -> a -> Bool
  quotLaw x y = 
    ((quot x y) * y + (rem x y)) == x

  divLaw :: Integral a => a -> a -> Bool
  divLaw x y = 
    ((div x y) * y + (mod x y)) == x

  prop_quotLaw :: Property
  prop_quotLaw = 
    forAll twoNonZeroes (\(x, y) -> quotLaw x y)

  prop_divLaw :: Property
  prop_divLaw = 
    forAll twoNonZeroes (\(x, y) -> divLaw x y)
  
  -- main :: IO ()
  -- main = quickCheck (quotLaw :: Integer -> Integer -> Bool) Fails
  -- main = quickCheck prop_quotLaw
  -- main = quickCheck prop_divLaw

  --------------------

  -- Checking if (^) is associative

  twoArbitrary :: (Integral a, Eq a, Arbitrary a) => Gen (a, a)
  twoArbitrary = do 
    a <- arbitrary
    b <- arbitrary
    return (a, b)

  threeArbitrary :: (Integral a, Eq a, Arbitrary a) => Gen (a, a, a)
  threeArbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

  -- powerCommutative :: (Integral a, Eq a) => a -> a -> Bool
  -- powerCommutative x y = (x ^ y) == (y ^ x)

  -- prop_power :: Property
  -- prop_power = 
  --   forAll twoArbitrary (\(x, y) -> powerCommutative x y)

  -- main :: IO ()
  -- main = quickCheck prop_power

  powerAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
  powerAssociative x y z = 
    x ^ (y ^ z) == (x ^ y) ^ z

  prop_associative :: Property
  prop_associative = 
    forAll (threeArbitrary :: Gen (Integer, Integer, Integer)) (\(x, y, z) -> powerAssociative x y z)

  -- main :: IO ()
  -- main = quickCheck prop_associative

  ------------------------------------------

  -- arbitraryList :: (Eq a, Arbitrary a) => Gen [a]
  -- arbitraryList = do 
  --   return $ listOf (Gen )

  prop_reverse_int :: [Int] -> Bool
  prop_reverse_int xs = (reverse . reverse) xs == (id xs)

  prop_reverse_str :: [String] -> Bool
  prop_reverse_str xs = (reverse . reverse) xs == (id xs)

  main :: IO ()
  main = do 
    quickCheck prop_reverse_int
    quickCheck prop_reverse_str