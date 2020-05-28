module Proper(
Variable(..)
,Equation(..)
,proper
,properList
,weaklyProper
,generateVars
,generateEq
) where

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
			 } deriving (Show, Ord, Eq)


--Modifying this to include parameters of E (for weak properness)
--Input : Instance (obvious). rSet, tSet are for weak properness and [Variable] is for edges
generateEq :: Instance -> [Variable] -> [Equation]
generateEq inst vars
	= [Equation k l i j neighbors| (k,l) <- (iPairs inst)
				, let d1= userD ( (iUsers inst) !! k ) , i <- [0..d1-1]
				, let d2= userD ( (iUsers inst) !! l ) , j <- [0..d2-1]
				, let neighbors = Set.fromList $ filter (eqBind k l i j) (vars)
				, k /= l  ]  
	where users = length (iUsers inst)


--Need to link equations in edges
-- After equations are generated. Generate variables and link them. 
generateVars :: Instance -> [Int] -> [Int] -> [Variable]
generateVars inst rSet tSet
	= [Variable u True i j | u <- tSet
					, let d_i = userD ((iUsers inst) !! u)
					, let n_i = userN ((iUsers inst) !! u)
					, i <- [0..(n_i - d_i - 1)]
					, j <- [0..d_i- 1 ] ]
	++ [Variable u False i j | u <- rSet
					, let d_i = userD ((iUsers inst) !! u)
					, let m_i = userM ((iUsers inst) !! u)
					, i <- [0..(m_i - d_i - 1)]
					, j <- [0..d_i-1] ]
	where users = length (iUsers inst)

--Detect Edges (Eq side)
eqBind :: Int -> Int -> Int -> Int -> Variable -> Bool
eqBind k l i j var = if ( vSender var == True && l == vUser var && vColumn var == j  )
			|| ( vSender var == False && k == vUser var && vColumn var == i )
			then	True
			else	False

--Properness Check
proper :: Instance -> Bool
proper inst = all (hallTest) eqList
	where	eqSubsets = powerset ( Set.fromList ( generateEq inst ( generateVars inst [0..k-1] [0..k-1]) ) )
		k = length (iUsers inst)
		eqList = Set.toList (Set.map (Set.toList) eqSubsets)


--Properness Check (List)
properList :: Instance -> Bool
properList inst = all (hallTest) eqSubsets
	where	eqSubsets = listPowerset (generateEq inst ( generateVars inst [0..k-1] [0..k-1]) )
		k = length (iUsers inst)


-- Given an instance, is it weakly proper?
weaklyProper :: Instance -> Bool
weaklyProper inst = properTestAll properChecks inst		--How?
	where	properChecks = [ proper' rSet tSet | rSet <- rSets, tSet <- tSets ]
		rSets = listPowerset [0..k-1]
		tSets = listPowerset [0..k-1]
		k = length (iUsers inst)

--weaklyProper helper
proper' ::  [Int] -> [Int] -> Instance -> Bool
proper' rSet tSet inst= hallTest (eqSubsets)
	where	eqSubsets = generateEq inst2 ( generateVars inst rSet tSet)
		k = length (iUsers inst)
		pairs = [ (k,l) | k <- rSet, l <- tSet]
		inst2 = Instance (iUsers inst) [(k,l) | (k,l) <- iPairs inst , (k,l) `elem` pairs]

--Partially Connected Proper check for an instance.
{-
pcProper ::  Instance -> Bool
pcProper inst = all (hallTest) (eqSubsets)
	where	eqSubsets = listPowerset ( generateEq inst ( generateVars inst [0..k-1] [0..k-1]) )
		k = length users
		inst = Instance users


--Partially Connected weakly proper check.
pcProper' :: [ (Int, Int) ] -> [Int] -> [Int] -> Instance -> Bool
-}

--Hall test for a set of Equations
hallTest :: [Equation] -> Bool
hallTest [] = True
hallTest eqs = ( neighborCount >= (length eqs) )
	where neighborCount = (Set.size (neighborhood eqs) )

-- Neighborhood of a set of Equations
neighborhood :: [Equation] -> Set.Set Variable
neighborhood [] = Set.empty
neighborhood (eq:[]) = eNeighbors eq
neighborhood (eq:eqs) = eqn `Set.union` eqsn
	where	eqn = eNeighbors eq
		eqsn = neighborhood eqs


--Analog of (all) but for weaklyProper
properTestAll :: [Instance -> Bool] -> Instance -> Bool
properTestAll [] inst = True
properTestAll (t:testList) inst = (t inst) && properTestAll testList inst
