
data Tree a = Node a (Tree a) (Tree a) | Nil deriving (Eq, Show)


preorder :: Eq a => Tree a -> [a]
preorder Nil = []
preorder (Node x left right) = x : preorder left ++ preorder right

inorder :: Eq a => Tree a -> [a]
inorder Nil = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

t = Node 1 (Node 2 (Node 4 Nil (Node 3 Nil Nil)) Nil) (Node 5 Nil (Node 6 Nil Nil))

