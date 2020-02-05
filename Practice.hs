{-# LANGUAGE FlexibleInstances #-}

module Practice where 


-- APPLICATIVE EXERCISES

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys 

y :: Maybe Integer 
y = lookup 2 $ zip xs ys 

summed :: Maybe Integer 
summed =  fmap sum ( (,) <$> x <*> y) 

-- FUNCTOR EXERCISES

data Parappa f g a 
  = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b 
  = IgnoreSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

data Notorious g o a t 
  = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = 
      Notorious go ga (fmap f gt)

data List a 
  = Nil 
  | Cons a (List a)

instance Functor List where 
  fmap _ Nil = Nil 
  fmap f (Cons v list) = Cons (f v) $ fmap f list

data GoatLord a 
  = NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3)
    = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a 
  = Halt 
  | Print String a 
  | Read { getFunc :: String -> a }

instance Functor TalkToMe where 
    fmap f Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read sa) = Read $ fmap f sa 

f = read :: String -> Int
-- x = Read read :: TalkToMe Int