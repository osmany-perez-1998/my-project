module Random where
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random, randomR, Random, randomIO)
import Control.Applicative ((<$>))

-- runRandom rand <seed>

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r