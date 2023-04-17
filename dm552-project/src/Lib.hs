{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( generateGame
  , simulateGame
  , countGames
  ) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Text.Read
import System.Random

type Card = String
type Position = (Integer, Integer)
type State = ([Card], [Position], [Position], Integer, Bool, Bool)
type Move = (Position, Position, Card)
type PlayerState = ([Card], [Position], Bool)

--contains all possible card moves for player1 and player2; first half of coordinates belongs to player1, the second is player2
playCards :: Position -> Card -> [Position]
playCards (x, y) "Tiger" = [(x-1,y),(x+2,y),(x+1,y),(x-2,y)]
playCards (x, y) "Dragon" = [(x-1,y-1),(x-1,y+1),(x+1,y-2),(x+1,y+2),(x+1,y+1),(x+1,y-1),(x-1,y+2),(x-1,y-2)]
playCards (x, y) "Frog" = [(x-1,y+1),(x+1,y-1),(x,y-2),(x+1,y-1),(x-1,y+1),(x,y+2)]
playCards (x, y) "Rabbit" = [(x-1,y-1),(x+1,y+1),(x,y+2),(x+1,y+1),(x-1,y-1),(x,y-2)]
playCards (x, y) "Crab" = [(x+1,y),(x,y-2),(x,y+2),(x-1,y),(x,y+2),(x,y-2)]
playCards (x, y) "Elephant" = [(x,y+1),(x,y-1),(x+1,y+1),(x+1,y-1),(x,y-1),(x,y+1),(x-1,y-1),(x-1,y+1)]
playCards (x, y) "Goose" = [(x,y+1),(x,y-1),(x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y-1)]
playCards (x, y) "Rooster" = [(x,y+1),(x,y-1),(x-1,y-1),(x+1,y+1),(x,y-1),(x,y+1),(x+1,y+1),(x-1,y-1)]
playCards (x, y) "Monkey" = [(x+1,y+1),(x-1,y+1),(x-1,y-1),(x+1,y-1),(x-1,y-1),(x+1,y-1),(x+1,y+1),(x-1,y+1)]
playCards (x, y) "Mantis" = [(x-1,y),(x+1,y+1),(x+1,y-1),(x+1,y),(x-1,y-1),(x-1,y+1)]
playCards (x, y) "Horse" = [(x-1,y),(x,y-1),(x+1,y),(x+1,y),(x,y+1),(x-1,y)]
playCards (x, y) "Ox" = [(x-1,y),(x,y+1),(x+1,y),(x+1,y),(x,y-1),(x-1,y)]
playCards (x, y) "Crane" = [(x+1,y),(x-1,y-1),(x-1,y+1),(x-1,y),(x+1,y+1),(x+1,y-1)]
playCards (x, y) "Boar" = [(x+1,y),(x,y-1),(x,y+1),(x-1,y),(x,y+1),(x,y-1)]
playCards (x, y) "Eel" = [(x,y+1),(x-1,y-1),(x+1,y-1),(x,y-1),(x+1,y+1),(x-1,y+1)]
playCards (x, y) "Cobra" = [(x,y-1),(x+1,y+1),(x-1,y+1),(x,y+1),(x-1,y-1),(x+1,y-1)]

-- all possible super moves
possibleSuperMoves :: (Num a1, Num a2) => (a1, a2) -> [(a1, a2)]
possibleSuperMoves (x,y) = [ (x+1,y),(x+1,y-1),(x,y-1),(x-1,y-1),
                            (x-1,y),(x-1,y+1),(x,y+1),(x+1,y+1) ]

-- card pool used for generateGame
cardsList :: [Card]
cardsList = ["Tiger", "Dragon", "Frog", "Rabbit", "Crab", "Elephant", "Goose", "Rooster",
                 "Monkey", "Mantis", "Horse", "Ox", "Crane", "Boar", "Eel", "Cobra"]
-- HELPER FUNCTIONS --

--used for generateGame
randoNo:: (Random a, RandomGen g, Num a) => a -> g -> (a, g)
randoNo n = randomR (0,n)

--used for simulateGame
dropSuperPrefix :: String -> String
dropSuperPrefix card | isPrefixOf "Super " card = drop (length "Super ") card
dropSuperPrefix card = card

--used for simulateGame
whoseTurn :: State -> [Move]
whoseTurn ([c1,c2,c3,c4,_], pawns, _, 0, usedSuper, _) = possibleMoves player1State (0)
  where player1State = ([c1,c2], pawns, usedSuper)
whoseTurn ([c1,c2,c3,c4,_], _, pawns, 1, _, usedSuper) = possibleMoves player2State (1)
  where player2State = ([c3,c4], pawns, usedSuper)

  --used for simulateGame
insideBoard :: Position -> Bool
insideBoard (x, y) = x >= 0 && x <= 4 && y >= 0 && y <= 4

--used for simulateGame
checkWin :: ([Position],[Position]) -> ([Position],[Position])
checkWin (pawns1, pawns2)
  | length pawns1 == 1 && elem (4,2) pawns1 = (pawns1,[])
  | length pawns2 == 1 && elem (0,2) pawns2 = ([],pawns2)
  | otherwise = (pawns1,pawns2)

-- used for generateGame
removeItem :: Eq a => [a] -> a -> [a]
removeItem list item = delete item list

-- used for generateGame
nextMove :: State -> StdGen -> Integer -> [Move] -> [Move]
nextMove state gen movesNeed finalList
  | ((length finalList == fromIntegral movesNeed) || null pawns1 || null pawns2) = finalList
  | otherwise = nextMove upState upGen movesNeed upList
  where upState = applyMove state (last upList)
        upList = finalList ++ upMoves
        (upMoves, upGen) = generateSomething (whoseTurn state) [] gen 1
        (cards, pawns1, pawns2, turn, super1, super2) = state

-- used for generateGame
generateSomething :: (Eq a, RandomGen t) => [a] -> [a] -> t -> Integer -> ([a],t)
generateSomething fromList toList genNum n
  | length toList == fromIntegral n = (toList,genNum)
  | otherwise = generateSomething (removeItem fromList (fromList !! i)) ((fromList !! i) : toList) g n
  where
    l = length fromList -1
    (i,g) = randomR (0,l) genNum

-- used to split playCards moves for player1 and player2
splitList :: [a] -> ([a], [a])
splitList myList = splitAt ((length myList + 1) `div` 2) myList

-- makes list of all states aka possible games
possibilities :: State -> Integer -> [State] -> [State]
possibilities state nMoves stateList
  | nMoves == 0 = stateList ++ [state]
  | null pawns1 || null pawns2 = stateList ++ [state]
  | otherwise = do
    let moves = whoseTurn state
    let upState = map (applyMove state) moves
    concatMap(\upd -> possibilities upd (nMoves-1) stateList) upState
    where (cards, pawns1, pawns2, turn, super1, super2) = state

-- MOVE FUNCTIONS --

-- determines moves based on used supers and player turn
possibleMoves :: PlayerState -> Integer -> [Move]
possibleMoves (cards, pawns, True) 0 = normalMoves
  where normalMoves = concatMap (cardMoves pawns) cards
possibleMoves (cards, pawns, False) 0 = normalMoves ++ superMoves pawns cards
  where normalMoves = concatMap (cardMoves pawns) cards
possibleMoves (cards, pawns, True) 1 = normalMoves
  where normalMoves = concatMap (cardMovesP2 pawns) cards
possibleMoves (cards, pawns, False) 1 = normalMoves ++ superMoves pawns cards
  where normalMoves = concatMap (cardMovesP2 pawns) cards

-- deals with super moves
superMoves :: [Position] -> [Card] -> [Move]
superMoves pawns cards =
  [(pos1, pos2, "Super " ++ card) |
   pos1 <- pawns,
   pos2 <- possibleSuperMoves pos1,
   insideBoard pos2,
   card <- cards]

-- finds all possible card moves for player1 and determines if they are legal
cardMoves :: [Position] -> Card -> [Move]
cardMoves pawns card = legalMoves
  where moves = concatMap (pawnMoves card) pawns
        legalMoves = filter (\(_, pos2, _) -> notElem pos2 pawns) moves

-- finds all possible card moves for player2 and determines if they are legal
cardMovesP2 :: [Position] -> Card -> [Move]
cardMovesP2 pawns card = legalMoves
  where moves = concatMap (pawnMovesP2 card) pawns
        legalMoves = filter (\(_, pos2, _) -> notElem pos2 pawns) moves

-- all possible pawn moves for player1; checks if they are inside the board
pawnMoves :: Card -> Position -> [Move]
pawnMoves card pos1 =
  [(pos1, pos2, card) | pos2 <- halfA, insideBoard pos2]
  where (halfA,halfB) = splitList (playCards pos1 card)

 -- all possible pawn moves for player2; checks if they are inside the board
pawnMovesP2 :: Card -> Position -> [Move]
pawnMovesP2 card pos1 =
    [(pos1, pos2, card) | pos2 <- halfB, insideBoard pos2]
    where (halfA,halfB) = splitList (playCards pos1 card)

-- applies given move to the current state and updates the state
applyMove :: State -> Move -> State
applyMove state move = newState
  where (cards, pawns1, pawns2, turn, super1, super2) = state
        (pos1, pos2, card) = move
        newCards = applyCardMove turn cards (dropSuperPrefix card)
        (newPawns1, newPawns2) = applyPawnMove turn pawns1 pawns2 pos1 pos2
        (finalPawns1, finalPawns2) = checkWin (newPawns1, newPawns2)
        (newUsedSuper1, newUsedSuper2) = applySuperMove turn super1 super2 card
        newTurn = mod (turn+1) 2
        newState = (newCards, finalPawns1, finalPawns2, newTurn, newUsedSuper1, newUsedSuper2)

-- applies card moves and rearranges the cards appropriately
applyCardMove :: Integer -> [Card] -> Card -> [Card]
applyCardMove 0 [c1,c2,c3,c4,c5] card
  | card == c1 = sort [c2,c5] ++ [c3,c4,card]
  | card == c2 = sort [c1,c5] ++ [c3,c4,card]
applyCardMove 1 [c1,c2,c3,c4,c5] card
  | card == c3 = [c1,c2] ++ sort [c4,c5] ++ [card]
  | card == c4 = [c1,c2] ++ sort [c3,c5] ++ [card]

-- applies supermoves and changes super status appropriately
applySuperMove :: (Eq a, Num a) => a -> Bool -> Bool -> String -> (Bool, Bool)
applySuperMove turn super1 super2 card
  | "Super " `isPrefixOf` card =
    case (turn, super1, super2) of
      (0, False, _) -> (True, super2)
      (1, _, False) -> (super1, True)
applySuperMove _ super1 super2 _ = (super1, super2)

applyPawnMove :: Integer -> [Position] -> [Position] -> Position -> Position -> ([Position], [Position])
applyPawnMove 0 pawns1 pawns2 pos1 pos2 = (playerPawns, opponentPawns)
  where (playerPawns, opponentPawns) = applyPawnMove1 pawns1 pawns2 temple0 pos1 pos2
        temple0 = (2, 4)
applyPawnMove 1 pawns1 pawns2 pos1 pos2 = (opponentPawns, playerPawns)
  where (playerPawns, opponentPawns) = applyPawnMove1 pawns2 pawns1 temple1 pos1 pos2
        temple1 = (2, 0)

applyPawnMove1 (playerMaster:playerPawns) (opponentMaster:opponentPawns) temple pos1 pos2
  | playerMaster == temple = (temple:playerPawns, [])
  | pos2 == opponentMaster = (movePawn (playerMaster:playerPawns) pos1 pos2, [])
  | otherwise = (movePawn (playerMaster:playerPawns) pos1 pos2, opponentMaster:(filter (/= pos2) opponentPawns))

-- adjusts pawns after move
movePawn :: [Position] -> Position -> Position -> [Position]
movePawn (master:pawns) pos1 pos2
  | master == pos1 = pos2:pawns
  | otherwise = master:(sort $ map replacePawn pawns)
      where replacePawn = \p -> if p == pos1 then pos2 else p

-- FILEPATH HELPER FUNCTIONS --

-- checks for possible move constructor
maybeMove :: String -> Maybe ((Integer, Integer), (Integer, Integer), String)
maybeMove x = readMaybe x :: Maybe ((Integer, Integer), (Integer, Integer), String)

-- converts to state constructor
convertState ::  Maybe ([String], [(Integer,Integer)], [(Integer, Integer)], Integer, Bool, Bool) -> State
convertState x = (cards, pawns1, pawns2, turn, super1, super2)
  where (cards, pawns1, pawns2, turn, super1, super2) = fromJust x

-- converts to move constructor
convertMove :: Maybe ((Integer,Integer),(Integer,Integer), String) -> Move
convertMove x = (from, to, card)
  where (from, to, card) = fromJust x

invalidState :: State -> Bool
invalidState (cards, pawns1, pawns2, turn, super1, super2)
  | length cards /= 5 = True
  | null pawns1 || length pawns1 > 5 = True
  | null pawns2 || length pawns2 > 5 = True
  | not (null (intersect pawns1 pawns2)) = True
  | (any ((==False) . (`elem` cardsList))) cards = True
  | (any ((==False) . insideBoard)) pawns1 = True
  | otherwise = False

-- checks for instances of an invalidformat
invalidFormat :: State -> Bool
invalidFormat state
  | cardsNotSorted state = True
  | pawnsNotSorted state = True
  | otherwise = False

-- checks if cards are sorted appropriately
cardsNotSorted :: State -> Bool
cardsNotSorted (cards, pawns1, pawns2, turn, super1, super2) =
  cards /= sortedCards
  where [c1,c2,c3,c4,c5] = cards
        sortedCards = sort[c1,c2] ++ sort[c3,c4] ++ [c5]

-- sorts cards according to assignment requirements
sortCards :: [Card] -> [Card]
sortCards [c1,c2,c3,c4,c5] = sort[c1,c2] ++ sort[c3,c4] ++ [c5]

-- checks if pawns are sorted appropriately
pawnsNotSorted :: State -> Bool
pawnsNotSorted (cards, pawns1, pawns2, turn, super1, super2)
  | pawns1 /= head pawns1 : (sort . tail) pawns1 = True
  | pawns2 /= head pawns2 : (sort . tail) pawns2 = True
  | otherwise = False

-- DRIVER FUNCTIONS --

simulateGameAux :: State -> [Move] -> String
simulateGameAux state [] = show state
simulateGameAux state (m:ms) =
  case whoseTurn state of
    moves | elem m moves -> simulateGameAux (applyMove state m) ms
    _ -> "InvalidMove" ++ " " ++ show m


countGamesAux :: State -> Integer -> String
countGamesAux state nMoves = show (length games, p1wins, p2wins)
  where games = possibilities state nMoves [] --
        p1wins = length $ filter (\(_,_,pawns2,_,_,_)-> null pawns2) games -- filters player1 wins from list of states
        p2wins =  length $ filter (\(_,pawns1,_,_,_,_)-> null pawns1) games -- filters player2 wins from lists of states

simulateGame :: FilePath -> IO String
simulateGame path = do
  file <- readFile path
  if file == "" then return "InvalidFormat"
    else do
    let stringArray = lines file
    let fileState = readMaybe (head stringArray) :: Maybe ([String], [(Integer,Integer)], [(Integer, Integer)], Integer, Bool, Bool)
    let realState = convertState fileState
    if isNothing fileState then return "InvalidFormat"
    else do
      if invalidState realState then return "InvalidState"
      else do
        if invalidFormat realState then return "InvalidFormat"
        else do
          let listofMaybeMoves = map maybeMove (tail stringArray)
          if any isNothing listofMaybeMoves then return (show realState)
          else do
            let listOfMoves = map convertMove listofMaybeMoves
            return (simulateGameAux realState listOfMoves)

generateGame :: Integer -> Integer -> String
generateGame s n = do
  --random cards, sorted
  let rando = mkStdGen (fromIntegral s)
  let (cards, genNum) = generateSomething cardsList [] rando 5
  let sortedCards = sortCards cards

  -- default pawn positions and supers, turn is random
  let pawns1 = [(0,2),(0,0),(0,1),(0,3),(0,4)]
  let pawns2 = [(4,2),(4,0),(4,1),(4,3),(4,4)]
  let (turn, genNum2) = randoNo 1 genNum :: (Integer, StdGen)
  let super1 = False
  let super2 = False
  let genState = (sortedCards, pawns1, pawns2, turn, super1, super2)

  -- converts state to pass in nextMove and whoseTurn
  let stringState = lines (show genState)
  let realState = convertState (readMaybe (head stringState))

  -- generates moves list based on the generated state
  let genMoves = nextMove realState genNum2 n []

  -- game output; lists inital state and possible moves for that inital state
  let genGame = show genState : map show genMoves

  unlines genGame

countGames :: Integer -> FilePath -> IO String
countGames n path = do
  file <- readFile path
  if null file then return "InvalidFormat"
  else do
    let stringArray = lines file
    let fileState = readMaybe (head stringArray) :: Maybe ([String], [(Integer,Integer)], [(Integer, Integer)], Integer, Bool, Bool)
    let realState = convertState fileState
    if isNothing fileState then return "InvalidFormat"
    else do
      if invalidState realState then return "InvalidState"
      else do
        if invalidFormat realState then return "InvalidFormat"
        else do
          return $ countGamesAux realState n




