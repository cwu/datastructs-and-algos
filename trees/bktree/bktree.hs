import Data.Array

data BKTree a = BKEmpty | BKNode [a] [BKEdge a] deriving (Show)
data BKEdge a = BKEdge Int (BKTree a) deriving (Show)

bkt_add :: Eq a => BKTree a -> [a] -> BKTree a
bkt_add BKEmpty xs = BKNode xs []
bkt_add (BKNode ys edges) xs = BKNode ys insert (map insertIntoEdge edges)
	where
		dist = editDist xs ys
		insertIntoEdge edge@(BKEdge d node) = if d == dist then BKEdge d (bkt_add node xs)
												else edge


editDist :: Eq a => [a] -> [a] -> Int
editDist xs ys = 
	let
		(m,n) = (length xs, length ys)
		x = array (1,m) (zip [1..] xs)
		y = array (1,n) (zip [1..] ys)

		table :: Array (Int, Int) Int
		table = array bnds [(ij, dist ij) | ij <- range bnds]
		bnds = ((0,0),(m,n))

		dist (i,0) = i
		dist (0,j) = j
		dist (i,j) = minimum [1 + table ! (i-1,j), 1 + table ! (i,j-1),
				if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]
	in table ! (m,n)
