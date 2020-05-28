
module Instance
( User(..)
, Instance(..)
, instancesOf
, instanceUser
) where

data User = User { userID :: Int
		, userN :: Int
		, userM :: Int
		, userD :: Int } deriving (Show)

data Instance = Instance [User] deriving (Show)

m_x = 5
n_x = 5


instancesOf ::  Int -> [ Instance  ]
instancesOf k 
	| k == 0  = []
	| otherwise = [ Instance [ User u (tupleN !! u) (tupleM !! u) (tupleD !! u) 
			| u <- [0..(k-1)] ] 
			| tupleN <- (tuplesN k) , tupleM <- (tuplesM k)
			, tupleD <- tuplesD k
			, valid tupleD tupleN tupleM k ] --filtering illegal elements

instanceUser :: Instance -> Int -> User
instanceUser (Instance xs) i = (xs !! i)

--Filters based on d_k < N_k && d_k < M_k
valid :: [Int] -> [Int] -> [Int] -> Int-> Bool
valid d n m k
	| k == 0 = True
	| otherwise = if ( d !! (k-1) < min ( n !! (k-1)) (m !! (k-1) ) ) 
	  	      then valid d n m (k-1)
	              else False

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
