import Data.List (nub)

data Edge a = E a a

data Graph a = G [Edge a]

g = G [E 1 2, E 2 3, E 3 4]

vertices :: Eq a => Graph a -> [a]
vertices (G g) = nub (concat [[x,y] | E x y <- g])



-- see if I can turn the graph directly into a monad to get the vertices via join
