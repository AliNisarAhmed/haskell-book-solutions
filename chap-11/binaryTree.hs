module BinaryTree where

  data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show, Ord)

  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  insert' b Leaf = Node Leaf b Leaf
  insert' b (Node left a right) 
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left a right)
    = Node (mapTree f left) (f a) (mapTree f right)

  testTree' :: BinaryTree Integer
  testTree' = 
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

  mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


  -- Comverting BinaryTree into Lists

  preorder :: BinaryTree a -> [a]
  preorder Leaf = []
  preorder (Node left a right) = [a] ++ preorder left ++ preorder right

  postorder :: BinaryTree a -> [a]
  postorder Leaf = []
  postorder (Node left a right) = postorder left ++ postorder right ++ [a]

  inorder :: BinaryTree a -> [a]
  inorder Leaf = []
  inorder (Node left a right) = inorder left ++ [a] ++ inorder right

  testTree :: BinaryTree Integer
  testTree = 
    Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

  
    -- Write foldr for Binary Tree

  foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldTree _ b Leaf = b
  foldTree f b (Node left a right) = bothFolded
    where 
      leftFolded = f a (foldTree f b left)
      bothFolded = foldTree f leftFolded right

      -- no sure if the order of folding matters

  -- foldTree f b (Node left a right) = bothFolded
  --   where
  --     rightFolded = f a (foldTree f b right)
  --     bothFolded = foldTree f rightFolded left


