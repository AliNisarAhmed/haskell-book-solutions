module BinaryTree where

  data BinaryTree a 
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

  unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
  unfold f x = 
    case f x of
      Just (a, b, c) -> Node (unfold f a) b (unfold f c)
      Nothing -> Leaf


  -- Tree Builder

  treeBuild :: Integer -> BinaryTree Integer
  treeBuild int = unfold f 0
    where 
      f x 
        | x == int = Nothing
        | otherwise= Just (x + 1, x, x + 1)
