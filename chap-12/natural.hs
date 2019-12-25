module Natural where

  data Nat
    = Zero
    | Succ Nat deriving (Eq, Show)

  natToInteger :: Nat -> Integer
  natToInteger Zero = 0
  natToInteger (Succ x) = 1 + natToInteger x

  integerToNat :: Integer -> Maybe Nat
  integerToNat x 
    | x < 0 = Nothing
    | otherwise = Just (getInt x) 
      where 
        getInt 0 = Zero
        getInt n = Succ $ getInt (n - 1)
