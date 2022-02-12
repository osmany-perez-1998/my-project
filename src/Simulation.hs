module Simulation where
import Debug.Trace(trace)
import Behaviour
import Configuration
import Board (Environment (Environment, seed, children, time), Object (Playpen), generatePlaypen, generateChildren, generateRobot, generateObst, generateDirt, findAllRobots)
import Random
import Data.List

generateEnvironment :: Environment
generateEnvironment =
  let
    env = Environment  [] [] [] [] [] 0 dimension 43
    freeCoords = [(x,y) | x <-[0 .. (fst dimension -1)], y<-[0 .. (snd dimension -1)]]
    rd =  (runRandom rand (seed env) `mod` length freeCoords)
    playpen = Playpen Nothing (freeCoords !! rd )
    (e1,freeCoords1) = generatePlaypen playpen childCount  (delete (freeCoords !! rd ) freeCoords) env
    (e2,freeCoords2) = generateChildren childCount freeCoords1 e1
    (e3,freeCoords3) = generateRobot robotCount freeCoords2 e2
    (e4,freeCoords4) = generateObst obstCount  freeCoords3 e3
    (e5,freeCoords5) = generateDirt dirtCount freeCoords4 e4
    in e5

moveChildren :: Environment -> Environment
moveChildren env =
  let
    children1 = children env
    in moveChildrenAux children1 env

moveChildrenAux:: [Object] -> Environment -> Environment
moveChildrenAux  unMovedChildren env =
  if null unMovedChildren
    then env
    else
      let
        child = head unMovedChildren
        env1 = childAction child env
        in moveChildrenAux (tail unMovedChildren) env1

moveRobot :: Int ->Environment -> Environment
moveRobot simType env =
  let
    robots1 = findAllRobots env
    in moveRobotAux simType robots1 env

moveRobotAux::Int-> [Object] -> Environment -> Environment
moveRobotAux simType unMovedRobots env =
  if null unMovedRobots
    then env
    else
      let
        robot = head unMovedRobots
        env1 = if simType == 0 then robotActionClosestChild robot env else robotActionImprove robot env
        in moveRobotAux simType (tail unMovedRobots) env1


simulation :: Int -> [Environment]
simulation  simType = simulationAux simType [generateEnvironment]

simulationAux ::Int-> [Environment ] -> [Environment ]
simulationAux simType envList =
  if time (last envList) >= totalTime
    then envList
    else
      let
        e1 = last envList
        e2 = moveChildren e1
        e3 = moveRobot simType e2
        e4 = e3{time = time e3 + 1}
        in simulationAux simType (envList ++ [e4])

    