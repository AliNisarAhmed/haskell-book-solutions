module Functions where

  data Quantum 
    = Yes
    | No
    | Both deriving (Eq, Show)


  -- since function application has cardinality of exponentiation
  -- convert function below should have a cardinality of 2 ^ 3 = 8

  -- NOTE: This measures all different possibilities of function convert
  -- Hence, these are all the possibilites of implement convert

  convert :: Quantum -> Bool
  convert Yes = True
  convert No = True
  convert Both = True

  convert Yes = False
  convert No = False
  convert Both = False

  convert Yes = True
  convert No = True
  convert Both = False

  convert Yes = True
  convert No = False
  convert Both = False

  convert Yes = False
  convert No = False
  convert Both = True

  convert Yes = False
  convert No = True
  convert Both = True

  convert Yes = True
  convert No = False
  convert Both = True

  convert Yes = False
  convert No = True
  convert Both = False
