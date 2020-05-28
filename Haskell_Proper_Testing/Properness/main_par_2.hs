import Instance
import Proper
import Control.Parallel
import System.Environment

main = do
	print ("Hits, first half:")
	print (par f3 (par f1 ( par f4( par f2 (pseq x ( filter(d) (zip [0..] (zipWith (==) f1 f2 ) ) ) ) ) ) ) )
	print ("Hits, second half:")
	print (par f3 (par f1 ( par f4( par f2 (pseq x ( filter(d) (zip [(l `div` 2)..] (zipWith (==) f3 f4 ) ) ) ) ) ) ) )
		where
			f1 = map (properList) ( firstHalf)
			f2 = map (weaklyProper) ( firstHalf )
			f3 = map (properList) ( secondHalf )
			f4 = map (weaklyProper) ( secondHalf )
			firstHalf = take (l `div` 2) x
			secondHalf = drop (l `div` 2) x
			x = instancesOf (3)
			l = length x
			d(a,b) = b==False
