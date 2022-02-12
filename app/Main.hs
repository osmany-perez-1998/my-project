module Main where

-- import Lib
import Simulation
import Configuration
import qualified Debug.Trace as Db
import Prelude
import Board (Environment(Environment),children,robots,Object(Robot),bfs, findAllRobots, RobotType (AlphaRobot))
import Behaviour
import Simulation (generateEnvironment, simulation, moveChildren, moveRobot)
import Behaviour (robotActionClosestChild)
import Configuration (simType)


main :: IO ()
main = print (simulation simType)




