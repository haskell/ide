data Tree a = Node a (Tree a) (Tree a) | Leaf a

rotateRight :: Tree a -> Tree a
rotateRight (Node v (Node v' l' r') r) =  Node v' l' (Node v r' r)
rotateRight t                          = t
