data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
      deriving (Show, Ord, Eq)


instance Functor BinaryTree where
  fmap f (Leaf) = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)


preorder :: BinaryTree a -> [a]

preorderaux t l = case t of
                    Leaf -> l
                    Node l d r-> (preorder l) ++ [d] ++ (preorder r)

preorder t = preorderaux t []

inorder :: BinaryTree a -> [a]
inorder t = inorderaux t []
inorderaux t l = case t of
                   Leaf -> l
                   Node l d r -> [d] ++ (inorder l) ++ (inorder r)


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b

foldTree _ a (Leaf) = a
foldTree f a (Node l d r) = foldTree f (f d (foldTree f a l)) r

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b

