module UsingQuickCheck where

  import Test.QuickCheck
  import Data.List (sort)

  genDivisor :: Gen Float
  genDivisor = arbitrary `suchThat` (/=0)

  half :: Fractional a => a -> a
  half x = x / 2

  halfIdentity :: Fractional a => a -> a
  halfIdentity = (*2) . half

  prop_half :: Property
  prop_half = forAll genDivisor (\x -> halfIdentity x == x)

  prop_halfIdentity :: Property
  prop_halfIdentity =
    forAll genDivisor (\x -> halfIdentity x == x)

  testHalf :: IO ()
  testHalf = do
    quickCheck prop_half
    quickCheck prop_halfIdentity

  -- 2

  listOrdered :: (Ord a) => [a] -> Bool
  listOrdered xs = 
    snd $ foldr go (Nothing, True) xs
    where
      go _ status@(_, False) = status
      go y (Nothing, t) = (Just y, t)
      go y (Just x ,t) = (Just y, x >= y)

  genList :: (Arbitrary a, Eq a) => Gen [a]
  genList = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/=a)
    c <- arbitrary `suchThat` (`notElem` [a, b])
    return [a, b, c]

  genListInt :: Gen [Int]
  genListInt = genList

  genListChar :: Gen [Char]
  genListChar = genList

  prop_listOrdered :: (Arbitrary a, Ord a, Show a) => Gen [a] -> Property
  prop_listOrdered genList = 
    forAll genList $ \x -> listOrdered (sort x)

  prop_listOrderedInt :: Property
  prop_listOrderedInt = prop_listOrdered genListInt

  prop_listOrderedChar :: Property
  prop_listOrderedChar = prop_listOrdered genListChar

  testListOrdered :: IO ()
  testListOrdered = do
    quickCheck prop_listOrderedInt
    quickCheck prop_listOrderedChar