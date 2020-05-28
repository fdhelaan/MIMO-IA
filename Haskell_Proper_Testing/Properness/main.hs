import Instance
import Proper
import Control.Parallel
import System.Environment


{-
main = x11 `par` x12 `par` x21 `par` x22 `pseq` print (zip [0..20000] ( (zipWith (==) x11 x21) ++  (zipWith (==) x12 x22) ) )
	where	x11 = map (properList) (take (10000) x)
		x12 = map (properList) (drop (10000) x)		--What if l is odd?
		x21 = map (weaklyProper) (take (10000) x)
		x22 = map (weaklyProper) (drop (10000) x)
-}

{-
main = do
	args <- getArgs
	let x = instancesOf (read (args !! 0) )
	let f1 = map (properList) x 
	let f2 = map (weaklyProper) x 
	let f3= (zip [0..] ( zipWith (==) f1 f2 ) )
	print f3
	print "Hits:"
	let f4 = filter (\(a,b) -> b==False) f3
	print f4
-}

main = 	do
	let y = instancesOf 4
	let z = filter (f y) (zip [0..] (map (properList) y) )
	print z
		where f y a = if (snd a == False) && (userD (iUsers ( y !! fst a ) !! 1) > 1) 
				&& (userD (iUsers ( y !! fst a ) !! 2) > 2) 
				&& (userD (iUsers ( y !! fst a ) !! 3) > 3) then True
			else False
