{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Behaviour where
import Board
import Random
import Data.Bifunctor
import Data.List (delete)
import Configuration
import qualified Debug.Trace as Db

--falta poner el environment

------Child-------------------------------------------

childMoves :: Environment -> Bool
childMoves e = time e `mod` t == 0 




moveChildtoFree :: Coord -> Object -> Environment -> Environment
moveChildtoFree p c e = e {children= delete c (children e) ++ [Child p]  }

moveChildtoObst :: Coord -> Object  -> Environment -> (Environment , Bool )
moveChildtoObst p c e =
  let
    dir :: Coord
    dir = Data.Bifunctor.bimap (fst p -) (snd p -) (location c)
    (e1,outcome) = moveObst (Obstacle p) dir e
    e2 = if outcome then e1{children = delete c (children e) ++ [Child p]} else e1
  in (e2,outcome )

moveObst :: Object -> Coord -> Environment -> (Environment, Bool)
moveObst obst dir e =
  let
    obstToPos :: Coord
    obstToPos = Data.Bifunctor.bimap ( fst dir + ) ( snd dir + ) (location  obst)
    e1=if validPos obstToPos e
        then
          let
            objs = objectsAt obstToPos e
            canMove = null objs || elem (Obstacle obstToPos) objs
            (e1, outcome) = if canMove
              then
                if null objs then (e,True) else moveObst (Obstacle obstToPos) dir e
              else (e,False)
            e2 = if outcome then e1 {obstacles = delete obst (obstacles e1) ++ [Obstacle obstToPos]} else e
            in (e2,outcome )
        else (e,False)
  in e1

placeDirt :: Int -> [Coord] -> Environment ->Environment
placeDirt adjChildCount pos e
  | adjChildCount <=1 =
    let
      rd = runRandom rand (seed e)
      e1 = e {seed = rd}
      dirtCount = rd `mod` 2
      e2 = placeDirtIn pos dirtCount e1
      in e2
  | adjChildCount <=2 =
    let
      rd = runRandom rand (seed e)
      dirtCount = rd `mod` 3
      e1 = e {seed = rd}
      e2 = placeDirtIn pos dirtCount e1
      in e2
  | adjChildCount<=3 =
    let
      rd = runRandom rand (seed e)
      dirtCount = rd `mod` 6
      e1 = e {seed = rd}
      e2 = placeDirtIn pos dirtCount e1
      in e2
  | otherwise = e

placeDirtIn :: [Coord] -> Int -> Environment -> Environment
placeDirtIn pos inputCount e =
  if inputCount ==0 || null pos
    then e
    else
      let
        rd = runRandom rand (seed e)
        index = rd `mod` length pos
        e1 = e {seed = rd, dirts = dirts e ++ [Dirt Nothing (pos !! index)]}
        e2 = placeDirtIn (delete (pos !! index) pos) (inputCount-1) e1
      in e2

childAction :: Object -> Environment -> Environment
childAction c e =
  if not (childMoves e)
    then e
    else
      let
        adjFree = freeAdjacentsCoord c e
        objAdj = objAdjacents c e
        adjChildCount = length [x | x <- objAdj, isChild x] + 1
        adjObst = [location x | x <- objAdj, isObstacle x]

        posMoves = adjFree ++ adjObst
        rd = runRandom rand (seed e)
        e1 = e {seed = rd }
        move = rd `mod` length posMoves
        env = if move < length adjFree
          then
            let
              auxEnv = moveChildtoFree (posMoves !! move) c e1
              aux1Env = placeDirt adjChildCount (delete (posMoves !! move) adjFree ++ [location c]) auxEnv
              in aux1Env
          else
            let
              (auxEnv,moved) = moveChildtoObst (posMoves !! move) c e1
              auxEnv1 = if moved
                then
                  placeDirt adjChildCount (adjFree ++ [location c]) auxEnv
                else
                  auxEnv
              in auxEnv1
        in env

-------------------------------------------------------------------

--------------------Robot------------------------------------------

-------------------------------------------------------------------


robotActionClosestChild :: Object -> Environment -> Environment
robotActionClosestChild r e =
  let
    c = case r of
      Robot _ _ c1 -> c1
      _-> (-1,-1)
    obj =head ( objectsAt c e )
    bfsData = bfs r [] [] e
    bfsReversed = Db.trace ("BFS:"++show bfsData++"\nReversed BFS:"++show (reverse bfsData)) (reverse bfsData)

    e1= case obj of
      -------------------Robot---------------------------------------------------
      Robot _ c _ ->
        case c of
          Just (Child _) ->
            let
              play = closestPlayFree bfsData e
              c1 = case play of
                Just(Playpen _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = robotActionClosestDirt r e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
              in e2
          _->
            let
              child = Db.trace (show(closestChildFree bfsData e)) (closestChildFree bfsData e)
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              path = Db.trace ("Path: "++show(if c1 == (-1,-1) then [] else path1 bfsReversed c1)) (if c1 == (-1,-1) then [] else path1 bfsReversed c1)

              e2
                | length path <=1 = robotActionClosestDirt r e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = Db.trace (show (path !! 1)) (path !! 1)
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot Nothing c2) c2 e3
                    in e4
              in e2
      ------------------------Robot en Corral-----------------------------------------
      Playpen (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = robotActionClosestDirt r e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen (Just(Child c1)) c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2
          _->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = robotActionClosestDirt r e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen Nothing  c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2

      ---------------------Robot en Dirt----------------------------------------------
      Dirt (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              play = closestPlayFree bfsData e
              c1 = case play of
                Just(Playpen _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = robotActionClosestDirt r e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
              in e2
          _->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = robotActionClosestDirt r e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e5
                    in e5
              in e2
      _ -> e
  in e1

 

robotActionClosestDirt :: Object -> Environment -> Environment
robotActionClosestDirt r e =
  let
    c = case r of
      Robot _ _ c1 -> c1
      _-> (-1,-1)
    obj =head ( objectsAt c e )
    bfsData = bfs r [] [] e
    bfsReversed = reverse bfsData

    e1= case obj of
      -------------------Robot---------------------------------------------------
      Robot _ c _ ->
        case c of
          Just (Child _) ->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
              in e2
          _->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot Nothing c2) c2 e3
                    in e4
              in e2
      ------------------------Robot en Corral-----------------------------------------
      Playpen (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen (Just(Child c1)) c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2
          _->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen Nothing  c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2

      ---------------------Robot en Dirt----------------------------------------------
      Dirt (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
              in e2
          _->
            let
              dirt = closestDirtFree bfsData e
              c1 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              path = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              e2
                | length path <=1 = e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e5
                    in e5
              in e2
      _ -> e
    in e1

robotActionImprove :: Object -> Environment -> Environment
robotActionImprove r e =
  let
    c = case r of
      Robot _ _ c1 -> c1
      _-> (-1,-1)
    obj =head ( objectsAt c e )
    bfsData = bfs r [] [] e
    bfsReversed = reverse bfsData

    e1= case obj of
      -------------------Robot---------------------------------------------------
      Robot _ c _ ->
        case c of
          Just (Child _) ->
            let
              play = closestPlayFree bfsData e
              c1 = case play of
                Just(Playpen _ c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e3
                    in e4
              in e2
          _->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = secureAddObject (Robot AlphaRobot Nothing c2) c2 e3
                    in e4
              in e2
      ------------------------Robot en Corral-----------------------------------------
      Playpen (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen (Just(Child c1)) c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2
          _->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = e --esto aqui tiene peste----
                | otherwise =
                  let
                    (c2,_,_) = path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Playpen Nothing  c1 ) e3
                    e5 = secureAddObject (Robot AlphaRobot Nothing  c2) c2 e4
                    in e5
              in e2

      ---------------------Robot en Dirt----------------------------------------------
      Dirt (Just(Robot a b _)) c->
        case b of
          Just (Child _) ->
            let
              play = closestPlayFree bfsData e
              c1 = case play of
                Just(Playpen _ c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = robotActionClosestDirt r e ----Peste----
                | length path >2 =
                  let
                    (c2,_,_) = path !! 2

                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
                | otherwise =
                  let
                    (c2,_,_) =  path !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e4
                    in e5
              in e2
          _->
            let
              child = closestChildFree bfsData e
              c1 = case child of
                Just(Child c) -> c
                _ -> (-1,-1)
              pathA1 = if c1 == (-1,-1) then [] else path1 bfsReversed c1

              dirt = closestDirtFree bfsData e
              c11 = case dirt of
                Just(Dirt _ c) -> c
                _ -> (-1,-1)
              pathA2 = if c11 == (-1,-1) then [] else path1 bfsReversed c1

              path = if length pathA1 <= length pathA2 && length pathA1 > 1 then pathA1  else pathA2

              e2
                | length path <=1 = robotActionClosestDirt r e  ---Peste----
                | otherwise =
                  let
                    (c2,_,_) = bfsData !! 1
                    e3 = deleteObject obj e
                    e4 = addObject (Dirt Nothing c) e3
                    e5 = secureAddObject (Robot AlphaRobot (Just (Child c2)) c2) c2 e5
                    in e5
              in e2
      _ -> e
  in e1







