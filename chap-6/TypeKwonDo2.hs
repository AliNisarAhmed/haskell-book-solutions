module TypeKwonDo2 where
  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk aToB a b =
    b' == b
    where 
      b' = aToB a

  arith :: Num b
        => (a -> b)
        -> Integer
        -> a 
        -> b
  arith aToB int a =
    (aToB a) + (fromIntegral int)

  -- fromIntegral :: (Integral b, Num a) => a -> b

  -- fromIntegral converts Integral to Num
  -- x :: Fractional a => a
  -- type MyNum = MyNum 

  -- newtype Nada
  --   = Nada MyNum deriving (Eq, Show)

  -- instance (Num a) => Fractional (Nada a) where
  --   (Nada x) / (Nada y) = Nada (x / y)
  --   recip (Nada x) = Nada (recip x)
  --   fromRational x = Nada (fromRational r)