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

main = do
	args <- getArgs
	let x = instancesOf (read (args !! 0) )
	let f1 = map (properList) x 
	let f2= (zip [0..] f1 )
	print f2
