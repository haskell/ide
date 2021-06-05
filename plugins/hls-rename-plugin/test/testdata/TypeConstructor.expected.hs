data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf a

rotateRight :: BinaryTree a -> BinaryTree a
rotateRight (Node v (Node v' l' r') r) =  Node v' l' (Node v r' r)
rotateRight t                          = t
