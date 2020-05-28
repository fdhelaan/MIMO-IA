import Instance
import Proper
import System.Environment



main = do
	args <- getArgs
	let y = read (args !! 1) :: Int
	let x = instancesOf (read (args !! 0) )
	print (x !! y)
