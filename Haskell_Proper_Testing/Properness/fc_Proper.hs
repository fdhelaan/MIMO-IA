{- Imports : -}
import qualified Data.Set as Set
import Instance
import Control.Monad

--Data types
data Variable = Variable { vUser :: Int
			 , vSender :: Bool
			 , vRow :: Int
			 , vColumn :: Int
			 } deriving (Eq, Ord,Show)

data Equation = Equation { ek ::Int
			 , el :: Int
			 , eRow :: Int
			 , eColumn :: Int
			 , eNeighbors :: Set.Set Variable --REPLACE WITH SETS
			 } deriving (Show)

--Modifying this to include parameters of E (for weak properness)
--Input : Instance (obvious). rSet, tSet are for weak properness and [Variable] is for edges
generateEq :: Instance -> [Int] -> [Int] -> [Variable] -> [Equation]
generateEq (Instance instanceRaw) rSet tSet vars
	= [Equation k l i j neighbors| k <- rSet, l <- tSet, l /= k
				, let d1= userD (instanceUser inst k ) , i <- [0..d1-1]
				, let d2= userD (instanceUser inst l ) , j <- [0..d2-1]
				, let neighbors = Set.fromList $ filter (eqBind k l i j) (vars)  ]  
	where users = length instanceRaw
	      inst = Instance instanceRaw

--Need to link equations in edges
-- After equations are generated. Generate variables and link them. 
generateVars :: Instance -> [Int] -> [Int] -> [Variable]
generateVars (Instance instanceRaw) rSet tSet
	= [Variable u True i j | u <- tSet
					, let d_i = userD (instanceUser inst u)
					, let n_i = userN (instanceUser inst u)
					, i <- [0..(n_i - d_i - 1)]
					, j <- [0..d_i- 1 ] ]
--					, let neighbors = Set.fromList (filter (senderBind u i j) eqs)
	++ [Variable u False i j | u <- rSet
					, let d_i = userD (instanceUser inst u)
					, let m_i = userM (instanceUser inst u)
					, i <- [0..(m_i - d_i - 1)]
					, j <- [0..d_i-1] ]
--					, let neighbors =Set.fromList (filter (receiverBind u i j) eqs)
	where users = length instanceRaw
	      inst = Instance instanceRaw

--Detect Edges (Eq side)
eqBind :: Int -> Int -> Int -> Int -> Variable -> Bool
eqBind k l i j var = if ( vSender var == True && l == vUser var && vColumn var == j  )
			|| ( vSender var == False && k == vUser var && vColumn var == i )
			then	True
			else	False

--Powerset
powerset :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerset = (Set.fromList) . fmap Set.fromList . listPowerset .(Set.toList)

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])
--

--Properness Check
proper :: Instance -> Bool
proper (Instance users) = all (hallTest) (varSubsets)
	where	varSubsets = listPowerset ( generateEq inst [0..k-1] [0..k-1] ( generateVars inst [0..k-1] [0..k-1]) )
		k = length users
		inst = Instance users

proper' ::  [Int] -> [Int] -> Instance -> Bool
proper' rSet tSet (Instance users)= hallTest (varSubsets)
	where	varSubsets =  generateEq inst rSet tSet ( generateVars inst rSet tSet) 
		k = length users
		inst = Instance users

hallTest :: [Equation] -> Bool
hallTest [] = True
hallTest eqs = ( neighborCount >= (length eqs) )
	where neighborCount = (Set.size (neighborhood eqs) )
--
neighborhood :: [Equation] -> Set.Set Variable
neighborhood [] = Set.empty
neighborhood (eq:[]) = eNeighbors eq
neighborhood (eq:eqs) = eqn `Set.union` eqsn
	where	eqn = eNeighbors eq
		eqsn = neighborhood eqs

-- Given an instance, is it weakly proper?
weaklyProper :: Instance -> Bool
weaklyProper (Instance users) = properTestAll properChecks inst		--How?
	where	inst = Instance users
		properChecks = [ proper' rSet tSet | rSet <- rSets, tSet <- tSets ]
		rSets = listPowerset [0..k-1]
		tSets = listPowerset [0..k-1]
		k = length users

--Analog of (all) but for weaklyProper
properTestAll :: [Instance -> Bool] -> Instance -> Bool
properTestAll (t:[]) inst = True
properTestAll (t:testList) inst = (t inst) && properTestAll testList inst

 
--Given an Instance, generate weakProperness cleaned instances
weakModify :: Instance -> [Instance]
weakModify (Instance users) = [ 
			Instance [ User i  (userN (users !! i) ) (userM (users !! i)) (userD (users !!i))  
			|i <- subset ] 
			| subset <- subsets ]
	where 	k = length users
		subsets = listPowerset [0..k]
