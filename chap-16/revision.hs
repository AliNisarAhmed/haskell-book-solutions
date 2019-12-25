module Revision where

  -- class Sumthin a where
  --   s :: a -> a

  -- class Else where
  --   e :: b -> f (g a b c)

  -- class Biffy where
  --   slayer :: e a b
  --          -> (a -> c)
  --          -> (b -> d)
          --  -> e c d

  -- class Impish v where
  --   impossibleKind :: v -> v a

  -- class AlsoImp v where
  --   nope :: v a -> v

  data FixMePls a
    = FixMe
    | Pls a deriving (Eq, Show)

  instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

  ---------------------------------------------


  data CountingBad a
    = Hsnbg Int a deriving (Eq, Show)

  instance Functor CountingBad where
    fmap f (Hsnbg n a) = Hsnbg (n + 1) (f a)

  data CountingGood a
    = H Int a deriving (Eq, Show)

  instance Functor CountingGood where
    fmap f (H int a) = H int (f a)

-------------------------------------------------

  a = fmap (+1) $ read "[1]" :: [Int]

  b = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])

  c = fmap (*2) (\x -> x - 2)

  d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

  -- e :: IO Integer
  e = let ioi = readIO "1" :: IO Integer
          changed = fmap read (fmap (("123" ++) . show) ioi)
      in  fmap (*3) changed

  -----------------------------------------------------


  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIdentity f = fmap id f == f

  functorCompose :: (Functor f, Eq (f c))
                 => (a -> b)
                 -> (b -> c)
                 -> f a
                 -> Bool
  functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

  f :: [Int] -> Bool
  f x = functorIdentity x