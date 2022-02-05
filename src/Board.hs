module Board where
import Data.List (delete)
import Control.Applicative (Alternative(empty))
import Random

type Coord = (Int, Int)

data Object
  = Child Coord
  | Obstacle Coord
  | Dirt (Maybe Object) Coord
  | Playpen (Maybe Object) Coord
  | Robot RobotType (Maybe Object) Coord
  deriving (Show,Eq)

data RobotType = AlphaRobot | BetaRobot
  deriving (Show,Eq)

location :: Object -> Coord
location a = case a of
  Robot _ _ b -> b
  Child b -> b
  Obstacle b -> b
  Dirt _ b -> b
  Playpen _ b -> b

data Environment = Environment
  {
    -- board :: [Object],
    robots :: [Object],
    children :: [Object],
    dirts :: [Object],
    playpens :: [Object],
    obstacles :: [Object],
    time :: Int,
    dimension :: (Int,Int),
    seed :: Int
  }
  deriving (Show)

allObjects :: Environment -> [Object]
allObjects a = concat [robots a, children a, dirts a, playpens a, obstacles a]

objectsAt :: Coord -> Environment -> [Object]
objectsAt a b = filter ((==a). location) (allObjects b)


samePos :: Object -> Object -> Bool
samePos a b = sameCoord (location a) (location b)

sameCoord :: Coord -> Coord -> Bool
sameCoord (x1, y1) (x2, y2) = (x1 == x2) && (y1 == y2)

isChild :: Object -> Bool
isChild c =
  case c of
    Child _ -> True
    _   -> False

isObstacle :: Object -> Bool
isObstacle o =
  case o of
    Obstacle _ -> True
    _ -> False

isDirt :: Object -> Bool
isDirt o =
  case o of
    Dirt _ _ -> True
    _ -> False

isFreeDirt :: Object -> Bool
isFreeDirt o =
  case o of
    Dirt Nothing _ -> True
    _ -> False

isFreePlay :: Object -> Bool
isFreePlay o =
  case o of
    Playpen Nothing _ -> True
    _ -> False

isRobotChild :: Object -> Bool
isRobotChild o =
  case o of
    Robot _ (Just(Child _)) _ -> True
    _ -> False


moveObject :: Object -> Object -> Environment -> Environment
moveObject a b c = addObject a $ deleteObject b c

addObject :: Object -> Environment -> Environment
addObject a b =
  case a of
    Child _ -> b { children= children b ++ [a]}
    Robot {}-> b {robots = robots b ++ [a]}
    Dirt _ _ -> b {dirts = dirts b ++ [a]}
    Obstacle _ -> b {obstacles = obstacles b ++[a]}
    Playpen _ _ -> b {obstacles = playpens b ++ [a] }

deleteObject :: Object -> Environment -> Environment
deleteObject a b =
  case a of
    Child _ -> b { children= delete a (children b)}
    Robot {}-> b {robots = delete a (robots b)}
    Dirt _ _ -> b {dirts = delete a (dirts b)}
    Obstacle _ -> b {obstacles = delete a (obstacles b)}
    Playpen _ _-> b {playpens = delete a (playpens b) }

adjacentDir :: Coord -> [Coord]
adjacentDir a =
  let
    x = fst a
    y = snd a
    in [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1), (x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

validPos :: Coord -> Environment -> Bool
validPos a b = fst a >=0 && fst a < fst (dimension b) && snd a >=0 && snd a < snd (dimension b)

objAdjacents :: Object  ->Environment -> [Object]
objAdjacents a b = concat [objectsAt x b | x <- adjacentDir (location a), validPos x b ]

freeAdjacentsCoord :: Object -> Environment -> [Coord]
freeAdjacentsCoord a b = [x | x <- adjacentDir (location a), validPos x b, null(objectsAt x b) ]

allAdjacentsCoord :: Object -> Environment -> [Coord]
allAdjacentsCoord a b = [x | x <- adjacentDir (location a), validPos x b]

--This is for use after BFS done--
secureAddObject:: Object -> Coord -> Environment -> Environment
secureAddObject obj c env =
  if null (objectsAt c env)
    then addObject obj env
    else
      let
        objF = head (objectsAt c env)
        e1 = case objF of
          Dirt _ _ ->
            let
              e2 = deleteObject objF env
              e3 = addObject (Dirt (Just obj) c) e2
              in e3
          Playpen _ _ ->
            let
              e2 = deleteObject objF env
              e3 = addObject (Playpen (Just obj) c) e2
              in e3
          Child _ ->
              case obj of
                Robot a b c ->
                  let
                    e3 = deleteObject objF env
                    e4 = addObject (Robot a (Just objF) c) e3
                    in e4
                _-> env
          _ -> env
        in e1


isRobot :: Object -> Bool
isRobot obj =
  case obj of
    Robot {} -> True
    _ -> False

findAllRobots :: Environment -> [Object]
findAllRobots e =
  children e ++ findAllRobotsIndirt (dirts e) ++ findAllRobotsInPlayPen (playpens e)


findAllRobotsIndirt :: [Object] -> [Object]
findAllRobotsIndirt allDirt =
  if null allDirt
    then []
    else
      let
        d = head allDirt
        (r, state) =
          case d of
            Dirt (Just(Robot a b c)) _ -> (Robot a b c, True)
            _ -> (Child (-1,-1), False)
        in if state then r : findAllRobotsIndirt (tail allDirt) else findAllRobotsIndirt (tail allDirt)

findAllRobotsInPlayPen :: [Object] -> [Object]
findAllRobotsInPlayPen allPlaypen =
  if null allPlaypen
    then []
    else
      let
        d = head allPlaypen
        (r, state) =
          case d of
            Playpen (Just(Robot a b c)) _ -> (Robot a b c, True)
            _ -> (Child (-1,-1), False)
        in if state then r : findAllRobotsIndirt (tail allPlaypen) else findAllRobotsInPlayPen (tail allPlaypen)



isBarrier :: Object-> Object-> Bool
isBarrier source obj =
  case source of
    Child _ ->
      case obj of
        Robot {} -> True
        Child _ -> True
        Playpen (Just Robot {})  _ -> True
        Playpen (Just (Child _)) _ -> True
        Obstacle _ -> True
        _ -> False
    Robot {}->  --aqui falta comprobar si el robot tiene a un ninno cargado, entonces el corral con ninno es una barrera too---
      case obj of
      Robot {} -> True
      Playpen (Just Robot {}) _ -> True
      Dirt (Just Robot{}) _ -> True
      Obstacle _ -> True
      _ -> False
    _-> False

inVisited :: Coord -> [(Coord,Coord,Int)] -> Bool
inVisited c l =
  not (null l) && (
    let
      aux =case head l of
        (c,_,_) -> True
    in aux || inVisited c (tail l))



bfs:: Object -> [(Coord,Coord,Int)]-> [(Coord, Coord,Int)] -> Environment -> [(Coord, Coord,Int)]
bfs obj queue visited env =
  if null queue && not (null visited)
    then visited
    else
      let
        q1 = if null queue then [(location obj,location obj,0)] else queue
        deq = head q1

        parentCoor = case deq of (p,_,_)-> p
        distance = 1 + case deq of (_,_,d) ->d
        adj = [ a | a<- adjacentDir parentCoor, validPos a env]

        adjValid = [(x, parentCoor, distance) | x<- adj ,
          not (isBarrier obj (let a = objectsAt x env in if null a then Dirt Nothing (0,0) else head a )) && not (inVisited x visited)]

        answer = bfs obj ((if length q1 == 1 then [] else tail q1) ++ adjValid) (visited ++ [deq]) env
        in answer

path1:: [(Coord,Coord ,Int )] -> Coord -> [(Coord ,Coord ,Int )]
path1 reversedBfsData c =
  if null reversedBfsData
    then []
    else
      let
        (x,y,_) = head reversedBfsData
        cAux = if x == c then y else (-1,-1)

        in if cAux == (-1,-1) then path1 (tail reversedBfsData) c else path1 (tail reversedBfsData) cAux ++ [head reversedBfsData]



closestChildFree :: [(Coord ,Coord ,Int)]->Environment -> Maybe Object
closestChildFree bfsData env =
  if null bfsData
    then Nothing
    else
      let
        h = head bfsData
        c = case h of (co,_,_) -> co
        obj = objectsAt c env
        in if null obj || not (isChild (head obj)) then closestChildFree (tail bfsData) env else Just (head obj)

closestDirtFree :: [(Coord ,Coord ,Int)]->Environment -> Maybe Object
closestDirtFree bfsData env =
  if null bfsData
    then Nothing
    else
      let
        h = head bfsData
        c = case h of (co,_,_) -> co
        obj = objectsAt c env
        in if null obj || not (isFreeDirt (head obj)) then closestDirtFree (tail bfsData) env else Just (head obj)

closestPlayFree :: [(Coord ,Coord ,Int)]->Environment -> Maybe Object
closestPlayFree bfsData env =
  if null bfsData
    then Nothing
    else
      let
        h = head bfsData
        c = case h of (co,_,_) -> co
        obj = objectsAt c env
        in if null obj || not (isFreePlay (head obj)) then closestPlayFree (tail bfsData) env else Just (head obj)


closestImprovement :: [(Coord ,Coord ,Int)]->Environment -> Maybe Object
closestImprovement bfsData env =
  if null bfsData
    then Nothing
    else
      let
        h = head bfsData
        c = case h of (co,_,_) -> co
        obj = objectsAt c env
        in if null obj || not (isFreeDirt (head obj) || isChild (head obj)) then closestImprovement (tail bfsData) env else Just (head obj)



generatePlaypen :: Object ->Int-> [Coord] -> Environment -> (Environment,[Coord])
generatePlaypen play count freeCoord env =
  let
    bfsData = bfs play [] [] env
    in generatePlaypenAux bfsData count freeCoord env

generatePlaypenAux :: [(Coord,Coord,Int)] ->Int ->[Coord] ->Environment -> (Environment,[Coord])
generatePlaypenAux bfsData count freeCoord env =
  if count == 0 || null bfsData || null freeCoord
    then (env,freeCoord)
    else
      let
        (c1,_,_) = head bfsData
        e1 = env{playpens = Playpen Nothing c1:playpens env}
        in generatePlaypenAux (tail bfsData) (count-1) (delete c1 freeCoord) e1



generateChildren :: Int -> [Coord] -> Environment -> (Environment,[Coord])
generateChildren count freeCoord env =
  if count == 0 || null freeCoord
    then (env,freeCoord)
    else
      let
        rd = runRandom rand (seed env) `mod` length freeCoord
        e1 = env {children = Child (freeCoord !! rd):children env}
        in generateChildren (count-1) (delete (freeCoord !! rd) freeCoord) e1

generateRobot :: Int -> [Coord] -> Environment -> (Environment,[Coord])
generateRobot count freeCoord env =
  if count == 0 || null freeCoord
    then (env,freeCoord)
    else
      let
        rd = runRandom rand (seed env) `mod` length freeCoord
        e1 = env {robots = Robot AlphaRobot Nothing (freeCoord !! rd):robots env}
        in generateRobot (count-1) (delete (freeCoord !! rd) freeCoord) e1

generateObst :: Int -> [Coord] -> Environment -> (Environment,[Coord])
generateObst count freeCoord env =
  if count == 0 || null freeCoord
    then (env,freeCoord)
    else
      let
        rd = runRandom rand (seed env) `mod` length freeCoord
        e1 = env {obstacles = Obstacle (freeCoord !! rd):obstacles env}
        in generateObst (count-1) (delete (freeCoord !! rd) freeCoord) e1

generateDirt :: Int -> [Coord] -> Environment -> (Environment,[Coord])
generateDirt count freeCoord env =
  if count == 0 || null freeCoord
    then (env,freeCoord)
    else
      let
        rd = runRandom rand (seed env) `mod` length freeCoord
        e1 = env {dirts = Dirt Nothing (freeCoord !! rd):dirts env}
        in generateDirt (count-1) (delete (freeCoord !! rd) freeCoord) e1
