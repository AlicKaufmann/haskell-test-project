data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

tr :: Tree Integer
tr_left = (Node 1) (Leaf 2) (Leaf 3)
tr_right = (Node 4) (Leaf 5) (Leaf 6)
tr = Node 8 tr_left tr_right

all_paths :: Tree a -> [[a]]
all_paths (Leaf x) = [[x]]
all_paths (Node x l r) = fmap (x:) (all_paths l)++(all_paths r)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r) 

double x = 2*x

doubled_tr = fmap double tr

