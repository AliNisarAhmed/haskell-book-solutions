module Unfolds where

  myIterate :: (a -> a) -> a -> [a]
  myIterate f x = 
    [x] ++ myIterate f (f x)
    
  myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  myUnfoldr f x = 
    case f x of 
      Just (a, b) -> a : (myUnfoldr f b)
      Nothing -> []
    
    
    -- [first] ++ myUnfoldr f second
    -- where
    --   applied = f x
    --   first = 
    --     case applied of
    --       Just (a, _) -> a
    --   second = 
    --     case applied of 
    --       Just (_, b) -> b

  betterIterate :: (a -> a) -> a -> [a]
  betterIterate f x = myUnfoldr g x 
    where 
      g y = Just (y, f y)
