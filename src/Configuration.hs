module Configuration where
import Board (Environment(dimension))

simType :: Int
simType = 0 
--simType = 0 Look for closest child and try to put it in a Playpen as a priority
--simType = 1 Look for either child or dirt, and put it in Playpen and clean it respectively.

dimension :: (Int, Int)
dimension = (7,7)

childCount :: Int
childCount = 3

robotCount :: Int
robotCount = 3

obstCount :: Int
obstCount = 3

dirtCount :: Int
dirtCount = 3

t :: Int
t = 3

totalTime :: Int
totalTime =30