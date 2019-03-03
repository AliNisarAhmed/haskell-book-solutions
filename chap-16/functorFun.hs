module FunWithFunctors where

  -- lms List (Maybe String)

  n = Nothing
  w = Just "woohoo"
  ave = Just "Ave"

  lms = [ave, n, w]

  replaceWithP = const 'p'


  -- (.) :: (b -> c) -> (a -> b) -> a -> c

  -- fmap :: Functor f => (m -> n) -> f m -> f n
  -- fmap :: Functor g => (n -> o) -> g n -> g o

  -- (.) :: ( (n -> o) -> g n -> g 0 ) -> ( (m -> n) -> f m -> f n ) -> 

  -- now lms ~ List (Maybe (List String))

  ha = Just ["Ha", "Ha"]
  lmls = [ha, Nothing, Just []]
