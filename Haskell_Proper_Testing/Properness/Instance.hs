module Instance
( User(..)
, Instance(..)
, instancesOf
, instanceUser
, listPowerset
, powerset
) where

import qualified Data.Set as Set
import Control.Monad


data User = User { userID :: Int
		, userN :: Int
		, userM :: Int
		, userD :: Int } deriving (Show)

data Instance = Instance { iUsers :: [User]
			, iPairs :: [(Int,Int)] } deriving (Show)

m_x = 5
n_x = 5



--Must change (i,j) as it forces fully connected.
instancesOf ::  Int -> [ Instance  ]
instancesOf k 
	| k == 0  = []
	| otherwise = [ Instance [ User u (tupleN !! u) (tupleM !! u) (tupleD !! u)
			| u <- [0..(k-1)] ] config
			| tupleN <- (tuplessN k) , tupleM <- (tuplessM k)
			, tupleD <- tuplessD k
			, valid tupleD tupleN tupleM k  --filtering illegal elements
			, config <- configurations k ]
		where	tuplessN k = tuplesN k
			tuplessM k = tuplesM k
			tuplessD k = tuplesD k

instanceUser :: Instance -> Int -> User
instanceUser inst i = ( (iUsers inst) !! i)

--Filters based on d_k < N_k && d_k < M_k
valid :: [Int] -> [Int] -> [Int] -> Int-> Bool
valid d n m k
	| k == 0 = True
	| otherwise = if ( d !! (k-1) < min ( n !! (k-1)) (m !! (k-1) ) ) 
	  	      then valid d n m (k-1)
	              else False


configurations :: Int -> [[(Int,Int)]]
configurations k = init (listPowerset [(j,i) | j <- [0..(k-1)], i <- [0..(k-1)], i /= j])

tuplesN :: Int -> [ [Int] ]
tuplesN k
	 = tuples k n_x []
	where first = [1 | u <- [1..k]]

tuplesM :: Int -> [ [Int] ]
tuplesM k
	 = tuples k m_x []
	where first = [1 | u <- [1..k]]

tuplesD :: Int -> [ [Int] ]
tuplesD k
	 = tuples k (min n_x m_x) []
	where first = [1 | u <- [1..k]]

--Recursive helper: x is current 'first'
-- z is the maximum for a digit (n_x, m_x, etc)
tuples :: Int -> Int ->[Int] -> [[Int]]
tuples k z x 
	| x == [] = first:(tuples k z (tupleStep first z) )
	| x == end = [end]
	| otherwise = x:(tuples k z (tupleStep x z))
	where end = [z | u <- [1..k]]
	      first = [1 | u <- [1..k]]


-- z = n_x, m_x... etc
step :: Int -> Int -> Int
step x z
	| x == z = z
	| otherwise = x+1


-- Input xs tuple, output : (xs + 1)
-- z = n_x, m_x ... etc
tupleStep :: [Int] -> Int -> [Int]
tupleStep xs z
	| xs == [] = []
	| otherwise = if last xs < z then (init xs)++[ step (last xs) z ]
		      else (tupleStep (init xs) z)++[1]

--Powerset
powerset :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerset = (Set.fromList) . fmap Set.fromList . listPowerset .(Set.toList)

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])
--

